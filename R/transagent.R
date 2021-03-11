TRANSAGENT = setRefClass(
  'TRANSAGENT', contains = 'TRANSPORT', 
  
  fields  = list(
    cdate   = 'character',
    tdns    = 'character',
    stns    = 'character',
    ctdn    = 'character',
    csts    = 'character',
    cstn    = 'character',
    cdir    = 'character',
    ctrn    = 'numeric',
    csch    = 'list',
    cdel    = 'numeric',
    reward  = 'numeric',
    rstate  = 'numeric',
    state   = 'matrix',
    queue   = 'data.frame',
    events  = 'data.frame',
    roster  = 'data.frame',
    options = 'data.frame',
    occupied = 'data.frame'
  ), 
  
  methods = list(
    goto   = function(time){
      callSuper(time)

      colheaders = c('isintransit', 'isdelayed', 'isindecision', 'cancelled', 'shortdeparted', 'shortarrived', 'bypassed', 'skipped', 'terminated')
      cdate   <<- as_date(ctime)
      tdns    <<- objects$timetable$history$TDN %>% unique
      jids    = paste(tdns, as_date(ctime), sep = '_')
      roster  <<- objects$timetable$history %>%   
        filter(caseID %in% jids) %>% 
        left_join(get.platform_map() %>% select(nextStatus = status, nextPlatform = platform), by = 'nextStatus') %>% 
        add_transit_column %>% 
        select(caseID, TDN, startTime, transit, status, station, platform, nextStatus, nextStation, nextPlatform, duration, endTime) 
        
      roster$station[roster$status == 'START']       <<- 'START'
      roster$nextStation[roster$nextStatus == 'END'] <<- 'END'
      
      tdns    <<- roster$TDN %>% unique
      stns    <<- roster$station %>% unique %>% na.omit
      tables$links <<- get.transition_probabilities.tdn() %>% group_by(status, nextStatus) %>% 
        summarise(totalFreq = sum(totalFreq))
      
      ####### Network state:
      state <<- matrix(data = 0, nrow = length(tdns), ncol = 10 + length(stns), dimnames = list(tdns, c(colheaders, stns, 'START')))
      #goto(ctime)
      hist = history %>% 
        filter(caseID %in% jids) %>% 
        left_join(obj$get.direction_map(), by = 'TDN') %>% 
        left_join(get.platform_map() %>% select(nextStatus = status, nextPlatform = platform), by = 'nextStatus') %>% 
        add_transit_column %>% 
        select(caseID, TDN, dir, startTime, transit, status, station, platform, nextStatus, nextStation, nextPlatform, duration, endTime)
      
      hist$station[hist$status == 'START']         <- 'START'
      hist$nextStation[hist$nextStatus == 'END']   <- 'END'
      
      events <<- hist %>% filter(startTime < ctime)
      
      if(nrow(events) > 0){
        
        events %>% filter(!is.na(station)) %>% 
          reshape2::dcast(TDN ~ station, fun.aggregate = last, value.var = 'startTime') %>% 
          column2Rownames('TDN') %>% 
          {!is.na(.)} %>% logical2integer -> net_state
        
        rn = rownames(net_state) %^% rownames(state)
        cn = colnames(net_state) %^% colnames(state)
        state[rn, cn] <<- net_state[rn, cn]
        
        events %>% group_by(TDN) %>% summarise(isintransit = last(transit)) %>% na.omit %>% 
          column2Rownames('TDN') -> transit_state
        
        rn = rownames(transit_state) %^% rownames(state)
        state[rn, 'isintransit'] <<- transit_state[rn, 'isintransit'] %>% as.numeric
        
        events %>% mutate(train_state = substr(status, 1, 7)) %>% group_by(TDN) %>% 
          summarise(train_state = last(train_state), latest_event = last(startTime)) %>% 
          left_join(roster%>% mutate(train_state = substr(status, 1, 7)) %>% 
                      select(TDN, train_state, scheduled = startTime), by = c('TDN', 'train_state')) %>% 
          mutate(delay = difftime(latest_event, scheduled, units = 'secs')) %>% 
          column2Rownames('TDN') -> delay_state
        
        rn = rownames(delay_state) %^% rownames(state)
        state[rn, 'isdelayed'] <<- (delay_state[rn, 'delay'] > 120) %>% as.numeric
        
        # difftime(ctime, events$startTime, units = 'secs') %>% order %>% first -> wmin 
        # state[paste0('X', events$TDN[wmin]), 'isindecision'] <- 1
        
        which(events$status == 'START' & events$nextStatus == 'END') -> wcan
        if(length(wcan) > 0){
          state[events$TDN[wcan] %>% unique, 'cancelled'] <<- 1
        }
        
        events %>% filter(status != 'START') %>% group_by(TDN) %>% summarise(start_station = first(station)) %>% ungroup %>% 
          left_join(
            roster %>% filter(status != 'START') %>% group_by(TDN) %>% summarise(scheduled_start_station = first(station)) %>% ungroup, by = 'TDN') %>%
          mutate(stations_match = start_station == scheduled_start_station) %>% 
          column2Rownames('TDN') -> sd_state
        rn = rownames(sd_state) %^% rownames(state)
        state[rn, 'shortdeparted'] <<- as.numeric(!sd_state[rn, 'stations_match'])
        
        events %>% filter(nextStatus == 'END') %>% select(TDN, last_station = station) %>% 
          left_join(
            roster %>% filter(nextStatus == 'END') %>% select(TDN, scheduled_last_station = station), by = 'TDN') %>%
          mutate(stations_match = last_station == scheduled_last_station) %>% 
          column2Rownames('TDN') -> sa_state
        rn = rownames(sa_state) %^% rownames(state)
        state[rn, 'shortarrived'] <<- as.numeric(!sa_state[rn, 'stations_match'])
        
        events %>% filter(station == 'RMD', nextStation == 'FSS') %>% select(TDN, station, nextStation) %>% 
          left_join(roster %>% 
                      filter(station == 'RMD', nextStation != 'RMD') %>% 
                      select(TDN, station, scheduled = nextStation), by = c('TDN', 'station')) %>% 
          mutate(stations_match = (nextStation == scheduled)) %>% 
          column2Rownames('TDN') -> bl_state
        
        events %>% filter(station == 'FSS', nextStation == 'RMD') %>% select(TDN, station, nextStation) %>% 
          left_join(roster %>% 
                      filter(station == 'FSS', nextStation != 'FSS') %>% 
                      select(TDN, station, scheduled = nextStation), by = c('TDN', 'station')) %>% 
          mutate(stations_match = (nextStation == scheduled)) %>% 
          column2Rownames('TDN') %>% rbind(bl_state) -> bl_state
        
        if(nrow(bl_state) > 0){
          rn = rownames(bl_state) %^% rownames(state)
          state[rn, 'bypassed'] <<- as.numeric(!bl_state[rn, 'stations_match'])
        }
        
        which(state[, 'cancelled'] %>% as.logical) %U% 
          which(state[, 'shortdeparted'] %>% as.logical) %U%
          which(state[, 'shortarrived'] %>% as.logical) %U%
          which(state[, 'bypassed'] %>% as.logical) -> walt
        
        walt_tdns = rownames(state)[walt]
        
        roster %>% filter(!(TDN %in% walt_tdns), TDN %in% events$TDN, transit == 1) %>% 
          select(TDN, station, nextStation) %>% 
          inner_join(events %>% filter(transit == 1) %>% select(TDN, station, act_nxtstn = nextStation), by = c('TDN', 'station')) %>% 
          filter(nextStation != act_nxtstn) -> shorts
        
        if(nrow(shorts) > 0){
          state[shorts$TDN, 'skipped'] <<- 1
        }
        
        events %>% filter(nextStatus == 'END') %>% pull(TDN) -> rn
        state[rn, 'terminated'] <<- 1
      }
      
      ####### station-platform occupancies:
      
      hist %>% filter(startTime < ctime) %>% 
        filter(status != 'START') %>% 
        mutate(location = paste(station, platform, sep = '.')) %>% 
        select(caseID, startTime, location) %>% 
        # left_join(tables$profile.station %>% select(station, headway), by = 'station') %>% 
        # mutate(startTime = startTime + headway) %>% 
        arrange(startTime) %>% 
        group_by(location) %>% summarise(mintime = last(startTime), service = last(caseID)) %>% ungroup %>% 
        column2Rownames('location') ->> occupied
      
      all = get.transition_durations.tdn() %>% filter(status != 'START') %>% 
        mutate(status = status %>% stringr::str_remove(pattern = 'Arr.') %>% stringr::str_remove(pattern = 'Dep.')) %>% 
        pull(status) %>% unique
      
      occupied[all %-% rownames(occupied), 'mintime'] <<- modelStart
      
      occupied$station <<- rownames(occupied) %>% substr(1,3)
      occupied$headway <<- occupied %>% left_join(tables$profile.station %>% select(station, headway), by = 'station') %>% pull(headway)
      assert(sum(is.na(occupied$headway)) == 0, 'some headways are not easy to find!')
      #occupied %<>% mutate(mintime = mintime + headway)
      
      ########## Build the queue:
      hist %>% 
        filter(caseID %in% jids) %>%  
        filter((status == 'START') & (startTime >= ctime)) ->> queue
      
      events %>% filter(endTime > ctime) %>% select(-status, -startTime, -station, -platform, -transit) %>% 
        mutate(status = nextStatus, startTime = endTime, station = nextStation, platform = nextPlatform) %>%
        add_transit_column %>% 
        spark.select(colnames(hist)) %>% rbind(queue) %>% 
        arrange(startTime) ->> queue
      
      ctdn <<- queue[1,'TDN']
      state[ctdn, 'isindecision'] <<- 1
      
      # ind = state[, 'isindecision'] %>% as.logical %>% which
      # assert(length(ind == 1))
      # rownames(state)[ind]
      csts  <<- queue[1,'status']  # Current status
      cstn  <<- queue[1,'station'] # current station
      ctrn  <<- queue[1,'transit']
      cdir  <<- tables$direction_map$dir[tables$direction_map$TDN == ctdn]  # current direction
      
      cdel <<- delay_state[ctdn, 'delay'] %>% as.numeric
      ctime  <<- queue$startTime[1]
      
      roster %>% filter(TDN == ctdn, station == cstn, transit == ctrn) %>% as.list ->> csch
      # assert(length(csch) == 1, 'no scheduled status')
      
      options <<- data.frame()
      # Action legend:
      # 0: Continue as scheduled
      # 1: Cancel the service
      # 2: Bypass Loop
      # 3: Short Departure
      # 4: Short Arrival
      # 5: Skip next station
      reduced_state()
      
    },
    
    get_options   = function(){
      if(is.empty(options) & (nrow(queue) > 0)){
        valid_stations = roster %>% filter(TDN == ctdn) %>% pull(station)
        last_scheduled_station  = valid_stations %>% last
        first_scheduled_station = valid_stations %>% first
        valid_stations %<>% unique %>% c('START', 'END')
        
        # options <<- tables$links %>% filter(status == csts) %>%
        #   left_join(get.station_map(), by = 'status') %>% 
        #   left_join(get.station_map() %>% rename(nextStatus = status, nextStation = station), by = 'nextStatus') %>% 
        #   filter((nextStation %in% valid_stations) | (nextStatus == 'END'))
        options <<- get.transition_probabilities.dir() %>% filter(status == csts, dir == cdir) %>%
          left_join(get.station_map(), by = 'status') %>%
          left_join(get.station_map() %>% rename(nextStatus = status, nextStation = station), by = 'nextStatus') %>%
          filter((nextStation %in% valid_stations) | (nextStatus == 'END'))
        
        if(is.empty(options)){options <<- data.frame(TDN = ctdn, status = csts, nextStatus == 'END', totalFreq = 1, cum_freq = 1, cum_prob = 1, station = cstn, nextStation = 'END')}
        options$nextStation[options$nextStatus == 'END'] <<- 'END'
        
        if(csch$nextStatus == 'END'){
          options <<- options %>% filter(nextStatus == 'END') %>% mutate(action = 0)
        }  else {
          options[which((options$status == 'START') & (options$nextStatus == 'END')), 'action'] <<- 1 
          options[which((options$status != 'START') & (options$nextStatus == 'END')), 'action'] <<- 4 
          options[which((options$status == 'START') & (options$nextStation != csch$nextStation)), 'action'] <<- 3
          options[which((options$station == 'FSS') & (options$nextStation == 'RMD')), 'action'] <<- 2 
          options[which((options$station == 'RMD') & (options$nextStation == 'FSS')), 'action'] <<- 2
          options[which(options$nextStation == csch$nextStation), 'action'] <<- 0
          options$action[is.na(options$action)] <<- 5
        }
        
        # short departure from the last scheduled stations and short arrival to the first scheduled stations are not allowed:
        tbd = c(which((options$action == 3) & (options$nextStation == last_scheduled_station)),
                which((options$action == 4) & (options$nextStation == first_scheduled_station)))
        
        if(length(tbd) > 0){
          options <<- options[-tbd, ]
        }
      }
      return(options)
    },
    take_action   = function(act = 0){
      
      if(nrow(queue) == 0){
        options <<- data.frame()
        return(NULL)
      }
      
      if(!act %in% valid_actions()){
        reward <<- - 10
        return(NULL)
      }
      
      selopt = get_options() %>% filter(action == act) %>% arrange(desc(totalFreq)) %>% head(1)
      if(nrow(selopt) != 1){
        debug(check)
        check(.self)
      }
      
      queue[1,] %>% 
        mutate(nextStatus = selopt$nextStatus, nextStation = selopt$nextStation) %>% 
        predict_durations_V2(obj) %>% correct_durations_PB(obj) %>% 
        mutate(duration = duration_prd, endTime = startTime + duration_prd) %>% 
        left_join(get.platform_map() %>% select(nextStatus = status, plt = platform), by = 'nextStatus') %>% 
        add_transit_column %>% mutate(nextPlatform = plt) %>% 
        select(-duration_prd, -sdTime, -plt) ->> queue[1,]
      
      ###### Check for headway rule violation:
      if(queue[1, 'nextStatus'] != 'END'){
        loc = paste(queue[1,'nextStation'], queue[1,'nextPlatform'], sep = '.')
        if(queue[1,'endTime'] < occupied[loc, 'mintime'] + occupied[loc, 'headway']){
          shifttime = T # Temporarily
          if(shifttime) {
            timeshift <- as.numeric(difftime(occupied[loc, 'mintime'], queue[1,'endTime'], units = 'secs')) + occupied[loc, 'headway']
            queue[1,'endTime'] <<- queue[1,'endTime'] + timeshift
          }
        }
        ##### Update occupancy table:
        occupied[loc, 'mintime'] <<- queue[1, 'endTime']
        occupied[loc, 'service'] <<- ctdn
      }
      
      ####### Update network status:
      state[ctdn, 'isintransit'] <<-  queue[1, 'transit']
      # We compute delay only if we go as scheduled
      if(act == 0){
        cdel <<- difftime(queue[1, 'endTime'], csch$endTime, units = 'secs') %>% as.numeric
        state[ctdn, 'isdelayed'] <<- (cdel > 120) 
      } else {
        cdel <<- 0
        if((act == 2) | (act == 5)){
          state[ctdn, 'bypassed'] <<- 1
        }
        if(act == 1){
          state[ctdn, 'cancelled'] <<- 1
        }
        if(act == 3){
          state[ctdn, 'shortdeparted'] <<- 1
        }
        if(act == 4){
          state[ctdn, 'shortarrived'] <<- 1
        }
      }
      
      if(csts == 'START'){
        state[ctdn, 'START'] <<- 1
      } else {
        state[ctdn, cstn] <<- 1
      }
      
      if(queue[1, 'nextStatus'] == 'END'){
        state[ctdn, 'terminated'] <<- 1
      }
      
      ####### Add event(transition) to the simulation eventlog
      events %>% rbind(queue[1,]) ->> events
      
      
      ###### Get Reward:
      # service alteration returns a negative score. Following schedule has a positive 0.2 score regardless of delay:
      if(act == 0){
        reward <<- 0.1
        if(state[ctdn, 'isdelayed']){
          reward <<- - 0.1
        }
      } else {
        reward <<- - 0.1
      }
      
      # if service is terminated, agent receives a negative score if being altered or arrived late:
      if(state[ctdn, 'terminated']){
        if(state[ctdn, 'cancelled']){
          reward <<- reward - 4.0
        } 
        else if (state[ctdn, 'shortdeparted'] | state[ctdn, 'shortarrived']){
          reward <<- reward - 1.0
        } 
        else if (state[ctdn, 'bypassed']){
          reward <<- reward - 0.5
        }
        else {
          assert(act == 0, 'action must be 0 here')
          if(cdel > 300){
            reward <<- reward - 5.0
          } else {
            reward <<- reward + 1.0
          }
        }
      }
      
      ####### Update queue and change current state:
      if(queue[1, 'nextStatus'] != 'END'){
        queue[1, 'status']    <<-  queue[1, 'nextStatus']
        queue[1, 'station']   <<-  queue[1, 'nextStation']
        queue[1, 'platform']  <<-  queue[1, 'nextPlatform']
        queue[1, 'startTime'] <<-  queue[1, 'endTime']
        queue[1, 'transit']   <<-  (length(grep(queue[1, 'status'], pattern = 'Dep')) == 1)
        queue %>% arrange(startTime) ->> queue
      } else {
        queue <<- queue[-1, ]
      }
      
      # Big reward:
      # if simulation ends (queue is empty), then compute overall reliability and delivery
      if(nrow(queue) == 0){
        perf = get_metrics(events %>% mutate(startDate = cdate), roster %>% mutate(startDate = cdate))
        reward <<- reward + 70*perf$delivery + 30*perf$reliability
        # EMPTY ALL PROPERTIES
        ctdn    <<- character()
        cstn    <<- character()
        cdir    <<- character()
        options <<- data.frame()
        return(NULL)
      }
      
      
      ctdn <<- queue[1,'TDN']
      
      state[, 'isindecision']  <<- as.numeric(rownames(state) == ctdn)
      csts  <<- queue[1,'status']  # Current status
      cstn  <<- queue[1,'station'] # current station
      ctrn  <<- queue[1,'transit'] # current transit
      cdir  <<- queue[1,'dir']     # current direction

      roster %>% filter(TDN == ctdn, station == cstn, transit == ctrn) %>% as.list ->> csch

      ctime <<- queue[1, 'startTime']
      
      cdel  <<- difftime(ctime, csch$startTime, units = 'secs') %>% as.numeric
      options <<- data.frame()
      reduced_state()
    },
    
    get_state_matrix  = function(){rstate %>% as.numeric %>% matrix(nrow = 1) %>% data.matrix},
    valid_actions = function(){
      optns = get_options()
      if(nrow(optns) > 0){
        return(optns %>% pull(action) %>% unique %>% as.integer) # You can take them with probabilities also
      } else {return(integer())}
    },
    reduced_state = function(){
      cloc = paste(csch$nextStation, csch$nextPlatform, sep = '.') # current location
      winc = which((tables$inc_full$caseID == queue[1, 'caseID']) & (tables$inc_full$incident_timestamp < ctime)) # which incidents?
      
      rstate <<- c(dir   = as.numeric(queue$dir[1] == 'Up'),
                   cbl   = as.numeric(((cstn == 'FSS') & (rstate['dir'] == 0)) | ((cstn == 'RMD') & (rstate['dir'] == 1))),
                   dly   = as.numeric(cdel > 0),
                   dly1m = as.numeric(cdel > 60),
                   dly2m = as.numeric(cdel > 120),
                   dly5m = as.numeric(cdel > 300),
                   dep   = as.numeric(ctrn == 1),
                   toc   = as.numeric(ctime < occupied[cloc, 'mintime']),
                   ccl   = as.numeric(csts == 'START'),
                   prm   = sum(tables$inc_full$primary_flag[winc]),
                   sec   = sum(tables$inc_full$secondary_flag[winc]))
      return(rstate)
    }
  )
)
 







EXPERIENCE = setRefClass(
  'EXPERIENCE',
  
  fields = list(
    objects  = 'list',
    memory   = 'data.frame',
    max_mem  = 'integer',
    discount = 'numeric',
    num_action  = 'integer',
    num_state   = 'integer'),
  
  methods = list(
    initialize = function(keras_model, max_memory = NULL, discount_rate = 0.95){
      library(keras)
      objects$model  <<- keras_model
      
      max_mem     <<- max_memory    %>% verify(c('numeric', 'integer'), lengths = 1, default = 100)  %>% as.integer
      discount    <<- discount_rate %>% verify(c('numeric'), lengths = 1, domain = c(0,1), default = 0.95)
      num_state   <<- get_input_shape_at(keras_model, node_index = 0)[[2]]
      num_action  <<- get_output_shape_at(keras_model, node_index = 0)[[2]]
    },
    
    remember = function(episode){
      if((length(episode$previous_state) == num_state) & (length(episode$current_state) == num_state)){
        nmem = nrow(memory)
        if(nmem == 0){
          memory      <<- matrix(numeric(2*num_state + 2), nrow = 1) %>% as.data.frame
        }
        memory[nmem + 1, 1:num_state]      <<- episode$previous_state
        memory[nmem + 1, 1:num_state + num_state] <<- episode$current_state
        memory[nmem + 1, 2*num_state + 1]  <<- episode$action 
        memory[nmem + 1, 2*num_state + 2]  <<- episode$reward 
        
        if(length(memory) > max_mem){
          memory <<- memory[-1, ]
        }
        memory <<- na.omit(memory)
        # memory[is.na(memory)] <<- -1
      }
    },
    
    predict_actions = function(envstate){
      predict(objects$model, envstate %>% matrix(nrow = 1) %>% data.matrix)
    },
    
    get_training_data = function(data_size = 10){
      mem_size  = nrow(memory)
      data_size = min(mem_size, data_size)
      memory_samples = mem_size %>% sequence %>% sample(size = data_size, replace = FALSE)
      
      inputs  = memory[memory_samples, 1:num_state] %>% as.matrix
      targets = predict(objects$model, inputs) %>% as.matrix
      
      Q_val   = predict(objects$model, memory[memory_samples, num_state + (1:num_state)] %>% as.matrix) %>% apply(1, max)
      
      actions = memory[memory_samples, 2*num_state + 1] %>% as.integer
      rewards = memory[memory_samples, 2*num_state + 2]
      # targets[, actions] <- rewards + discount*Q_val
      
      for(i in sequence(data_size)){
        targets[i, actions[i] + 1] <- rewards[i] + discount*Q_val[i]
      }
      return(list(inputs = inputs, targets = targets))
    }
  )
)  



jump = function(obj, experience, n_step = 100, show_progress = T){
  
  step       = 0
  envstate   = obj$reduced_state()
  if(show_progress){pb = txtProgressBar(min = 1, max = n_step, style = 3)}
  
  valid_acts    = obj$valid_actions()
  while((length(valid_acts) > 0) & (step < n_step)){
    prev_envstate = envstate
    if(runif(1) < 0.05){
      action = sample(0:5, size = 1)
    } else {
      action = experience$predict_actions(prev_envstate) %>% order(decreasing = T) %>% {.-1} %>% first
    }
    # Apply selected action, get reward and generate new envstate:
    obj$take_action(action)
    envstate = obj$reduced_state()
    episode  = list(previous_state = prev_envstate, current_state = envstate, action = action, reward = obj$reward)
    experience$remember(episode)
    step = step + 1
    valid_acts    = obj$valid_actions()
    if(show_progress){
      setTxtProgressBar(pb, step)
      cat(' Step: ', step, ' Time: ', as.character(obj$now()))
    }
  }
  return(step)
}  


qtrain = function(model, obj, parameters = list()){
  n_epoch = parameters$n_epoch %>% verify(c('integer', 'numeric'), lengths = 1, default = 150) %>% as.integer
  max_mem = parameters$max_mem %>% verify(c('integer', 'numeric'), lengths = 1, default = 10000)  %>% as.integer
  dt_size = parameters$dt_size %>% verify(c('integer', 'numeric'), lengths = 1, default = 1000)  %>% as.integer
  numstep = parameters$numstep %>% verify(c('integer', 'numeric'), lengths = 1, default = 100)  %>% as.integer
  
  experience = EXPERIENCE(keras_model = model, max_memory = max_mem)
  
  for(epoch in sequence(n_epoch)){
    random_time = as.POSIXct(obj$cdate) + runif(1, 10000, 80000)
    obj$goto(random_time)
    
    envstate   = obj$get_state_matrix()
    n_episodes = 0
    success    = T
    
    while(success & (length(obj$valid_actions()) > 0)){
      jumped_steps = try(jump(obj, experience, numstep), silent = T)
      success = !inherits(jumped_steps, 'try-error')
      if(success){
        n_episodes = n_episodes + jumped_steps
        loss       = teach(obj, experience, model, dt_size)
        cat('\n', 'Epoch: ', epoch,'/', n_epoch, ' Loss: ', loss, ' Episodes: ', n_episodes, 'env time: ', as.character(obj$now()), '\n')
      }
    }
  }
  return(model)
}

teach = function(obj, experience, model, n_train_data = 100){
  # Train neural-network model:
  training_set = experience$get_training_data(n_train_data)
  history      = model %>% fit(x = training_set$inputs, y = training_set$targets, epochs = 100, batch_size = 32, verbose = 0)
  model %>% evaluate(x = training_set$inputs, y = training_set$targets, verbose = 0)
}  
