# TRANSPORT
TRANSPORT = setRefClass('TRANSPORT', contains = 'TRANSYS', fields = list(ctime = 'POSIXct', objects = 'list'), methods = list(
  feed.events = function(eventlog, JID_col = 'JID', station_col = 'station', event_type_col = 'event_type', event_time_col = 'event_time', departure_tag = 'departure', arrival_tag = 'arrival', platform_col = NULL){
    assert(eventlog %>% pull(event_type_col) %<% c(departure_tag, arrival_tag), 'Unknown event type observed in the eventlog!')
    et_map = c('Dep', 'Arr') %>% {names(.) <- c(departure_tag, arrival_tag);.}
    eventlog[, event_type_col] <- et_map[eventlog %>% pull(event_type_col)]

    if(!is.null(platform_col)){
      eventlog[, 'status'] <- paste(eventlog %>% pull(event_type_col), eventlog %>% pull(station_col), eventlog %>% pull(platform_col), sep = '.')
    } else {
      eventlog[, 'status'] <- paste(eventlog %>% pull(event_type_col), eventlog %>% pull(station_col), sep = '.')
    } 
    
    eventlog %<>% rename(station = station_col)

    feed.eventlog(eventlog, caseID_col = JID_col, startTime = event_time_col, extra_col = c('station', 'line', 'direction', 'platform'), add_start = T)
    
    if(is.null(tables$profile.station)){
      tables$profile.station <<- history %>% distinct(station)
    }
    
    if(!'rank' %in% colnames(tables$profile.station)){
      tables$profile.station <<- tables$profile.station %>% 
        mutate(rank = station %>% factor(levels = get.longest_station_path()) %>% as.integer)
    }

    history <<- history %>% 
      left_join(get.station_map() %>% select(nextStatus = status, nextStation = station), by = 'nextStatus')
      # mutate(nextStation = nextStatus %>% stringr::str_remove('Arr.') %>% stringr::str_remove('Dep.') %>% substr(1, 3))
      # todo: nextStation should not be created from status column. why?!
    
    ctime <<- min(history$startTime)
  },
  
  feed.timetable = function(eventlog, JID_col = 'JID', station_col = 'station', event_type_col = 'event_type', event_time_col = 'event_time', departure_tag = 'departure', arrival_tag = 'arrival', platform_col = NULL){
    assert(eventlog %>% pull(event_type_col) %<% c(departure_tag, arrival_tag), 'Unknown event type observed in the eventlog!')
    et_map = c('Dep', 'Arr') %>% {names(.) <- c(departure_tag, arrival_tag);.}
    eventlog[, event_type_col] <- et_map[eventlog %>% pull(event_type_col)]
    
    if(!is.null(platform_col)){
      eventlog[, 'status'] <- paste(eventlog %>% pull(event_type_col), eventlog %>% pull(station_col), eventlog %>% pull(platform_col), sep = '.')
    } else {
      eventlog[, 'status'] <- paste(eventlog %>% pull(event_type_col), eventlog %>% pull(station_col), sep = '.')
    } 
    
    eventlog %<>% rename(station = station_col)
    objects$timetable <<- new('TRANSYS')
    objects$timetable$feed.eventlog(eventlog, caseID_col = JID_col, startTime = event_time_col, extra_col = c('station', 'platform'))
    objects$timetable$history <<- objects$timetable$history %>% 
      mutate(
        nextStation = nextStatus %>% stringr::str_remove('Arr.') %>% stringr::str_remove('Dep.'))
  },
  
  get.stations = function(){
    if(is.null(report$stations)){
      report$stations <<- history %>% filter(selected) %>% pull(station) %>% unique
    }
    return(report$stations)
  },
  
  feed.journeys = function(journeys, JID_col = 'JID', TDN_col = 'TDN'){
    feed.case_features(journeys %>% rename(TDN = TDN_col), caseID_col = JID_col)
    history <<- history %>% left_join(tables$profile.case %>% select(caseID, TDN), by = 'caseID')
    objects$timetable$history <<- objects$timetable$history %>% left_join(tables$profile.case %>% select(caseID, TDN), by = 'caseID')
  },
  
  filter.reset = function(){
    callSuper()
    if(!is.null(tables$profile.incident)){
      tables$profile.incident$selected <<- T
    }
  },
  
  feed.incidents = function(incidents, id_col = 'incident_id', type_col = 'incident_type', time_from_col = 'start_time', time_to_col = 'end_time', station_from_col = 'station_from', station_to_col = 'statio_to', JID_col = 'jid'){
    # todo: make sure incident ids are unique
    # 
    filter.reset()
    incidents %>% rename(incid = id_col, inctype = type_col, time_from = time_from_col, time_to = time_to_col, 
                         st_from = station_from_col, st_to = station_to_col, caseID = JID_col) %>% 
      filter(time_from > modelStart, time_to < modelEnd, st_from %in% get.stations(), st_to %in% get.stations()) %>% 
      mutate(incdate = as_date(time_from), selected = T) ->> tables$profile.incident
  },
  
  feed.stations = function(stf, station_col = 'station_id', lat_col = NULL, long_col = NULL){
    # todo: if the profile already exists, check colnames are different. What to do for identical colnames?
    stf %<>% rename(station = station_col)
    if(!is.null(lat_col)){stf %<>% rename(latitude = lat_col)}
    if(!is.null(long_col)){stf %<>% rename(longitude = long_col)}
    
    tables$profile.station <<- tables$profile.station %>% 
      full_join(stf, by = 'station')
    
    tables$profile.station$headway <<- 120
    tables$profile.station$headway[(tables$profile.station$rank > 5)  & (tables$profile.station$rank < 13)] <<- 150
    tables$profile.station$headway[(tables$profile.station$rank > 12) & (tables$profile.station$rank < 25)] <<- 180
    tables$profile.station$headway[tables$profile.station$rank > 24] <<- 300
  },
  
  goto = function(dt){
    ctime <<- dt %>% as.POSIXct
    filter.event(startTime < ctime)
  },
  
  now = function(){ctime},
  
  filter.case = function(...){
    callSuper(...)
    if(!is.null(objects$sim)){objects$sim$filter.case(...)}
    if(!is.null(objects$timetable)){objects$timetable$filter.case(...)}
    if(!is.null(tables$profile.incident)){
      all_dates = history[history$selected, 'startDate'] %>% unique
      tables$profile.incident$selected <<- tables$profile.incident$incdate %in% all_dates
    }
  },
  
  filterJourney = function(TDNs = NULL, dates = NULL){
    if(is.null(TDNs)){TDNs = history$TDN %>% unique}
    if(is.null(dates)){dates = history$startDate %>% unique}
    caseids = TDNs %>% lapply(function(x) paste(x, dates, sep = '_')) %>% unlist
    filter.case(IDs = caseids)
  },
  
  get.incidents = function(){
    tables$profile.incident %>% filter(selected)
  },
  
  get.direction_map = function(){
    if(is.null(tables$direction_map)){
      if(is.null(tables$jp_full)){
        jp = tables$profile.case
      } else {
        jp = tables$jp_full
      }
      jp %>% distinct(TDN, dir) ->> tables$direction_map
    }
    return(tables$direction_map)
  },
  
  get.transition_probabilities.dir = function(){
    if(is.null(tables[['transition_probabilities.dir']])){report[['transition_probabilities.dir']] <<- tables[['transition_probabilities.dir']]}
    
    if(is.null(report[['transition_probabilities.dir']])){
      get.transition_probabilities.tdn() %>% 
        left_join(get.direction_map(), by = 'TDN') %>% 
        group_by(dir, status, nextStatus) %>% 
        summarise(totalFreq = sum(totalFreq)) %>% 
        ungroup ->> report[['transition_probabilities.dir']]
    }
    return(report[['transition_probabilities.dir']])
  },
  
  get.transition_probabilities.tdn = function(){
    if(!is.null(tables[['transition_probabilities.tdn']])){report[['transition_probabilities.tdn']] <<- tables[['transition_probabilities.tdn']]}
    if(is.null(report[['transition_probabilities.tdn']])){
      possibilities = data.frame(status = 'START', TDN = unique(objects$timetable$history$TDN), nextStatus = 'END')
      objects$timetable$history %>% select(status, TDN, nextStatus) %>% 
        rbind(history %>% select(status, TDN, nextStatus)) %>% 
        rbind(possibilities) %>% 
        group_by(status, TDN, nextStatus) %>% 
        summarise(totalFreq = length(TDN)) %>% na.omit %>% 
        select(TDN, status, nextStatus, totalFreq) %>% arrange(TDN, status, nextStatus) %>% 
        group_by(TDN, status) %>% mutate(cum_freq = cumsum(totalFreq)) %>% 
        mutate(cum_prob = cum_freq/sum(totalFreq)) %>% ungroup() %>% 
        select(TDN, status, nextStatus, totalFreq, cum_prob) ->> report[['transition_probabilities.tdn']]
    }
    return(report[['transition_probabilities.tdn']])
  },

  get.transition_durations.tdn = function(){
    if(!is.null(tables[['transition_durations.tdn']])) report[['transition_durations.tdn']] <<- tables[['transition_durations.tdn']]
    if(is.null(report[['transition_durations.tdn']])){
      history %>% 
        group_by(status, nextStatus, TDN) %>% 
        summarise(meanTime = mean(duration, na.rm = T), sdTime = sd(duration, na.rm = T)) %>% 
        select(status, nextStatus, TDN, meanTime, sdTime) %>% 
        arrange(status, nextStatus, TDN) %>% na2zero ->> report[['transition_durations.tdn']]
    }
    return(report[['transition_durations.tdn']])
  },
  
  get.station_map = function(){
    if(is.null(tables$station_map)){
      tt = chif(is.empty(objects$timetable$history), NULL, objects$timetable$history %>% select(status, station))
      history %>% select(status, station) %>% rbind(tt) %>% 
        filter(status != 'START') %>% distinct(status, station) -> stmap
      
      assert(sum(duplicated(stmap)) == 0)
      
      tables$station_map <<- stmap
    }
    return(tables$station_map)
  },

  get.tdn_map = function(){
    if(is.null(tables$tdn_map)){
      tdnmap <- objects$timetable$history %>% distinct(caseID, TDN)
      # profile.case %>% distinct(caseID, TDN)
      assert(sum(duplicated(tdnmap)) == 0)
      tables$tdn_map <<- tdnmap
    }
    return(tables$tdn_map)
  },

  get.platform_map = function(){
    if(is.null(tables$platform_map)){
      tables$platform_map <<- history %>% filter(status != 'START') %>% distinct(status, platform)
    }
    return(tables$platform_map)
  },
  
  # run.simulation = function(until = NULL, TDNs = NULL, headway = T, headway_seconds = 120, ...){
  #   if(is.null(until)) until = ctime + 24*3600 else until = as.time(until)
  #   assert(until > ctime, "Cannot simulate to past! Argument 'until' must be greater than current time")
  #   lvls   = tables$profile.station %>% arrange(rank) %>% pull(station)
  # 
  #   if(is.empty(TDNs)){
  #     tables$future <<- history %>% filter(startTime > ctime) # temporary
  #     
  #     future_elog    <- callSuper(start_dt = ctime, target_dt = until, event_generator = metrotrain_next_event_generator, time_generator = metrotrain_transition_time_generator, ...)
  #   }
  #   else {
  #     future_elog = NULL
  #     tdns_ordered = history %>% filter(startTime > ctime, startTime < until) %>% group_by(TDN) %>% 
  #       summarise(startTime = min(startTime)) %>% filter(TDN %in% TDNs) %>% arrange(startTime) %>% pull(TDN)
  #     
  #     for(tdn in tdns_ordered){
  #       cat('Running Simulation for TDN: ', tdn, '... ')
  #       filterJourney(TDNs = tdn)
  #       fresh_arrivals <- history %>% filter(selected) %>% filter(startTime > ctime, startTime < until) %>% group_by(caseID) %>% summarise(startTime = first(startTime), status = first(status))
  #       objects$future_elog <<- future_elog
  #       future_elog = rbind(future_elog, callSuper(start_dt = ctime, target_dt = until, new_starts = fresh_arrivals, event_generator = metrotrain_next_event_generator, time_generator = metrotrain_transition_time_generator, station_map = get.station_map(), platform_map = get.platform_map(), tdn_map = get.tdn_map(), station_levels = lvls, ...))
  #       cat('Done!', tdn, '\n')
  #     }   
  #   }
  # 
  #   if (is.empty(future_elog)){
  #     cat('Simulation returned no results!', '\n')
  #   } else {
  #     if(headway){
  #       future_elog %<>% rename(endTime = nxtTrTime) %>% correct_events(obj = .self, headway_seconds = headway_seconds)
  #     }
  # 
  #     objects$sim <<- new('TRANSYS')
  #     objects$sim$feed.eventlog(future_elog, add_start = T, remove_sst = T)
  #     objects$sim$history <<- objects$sim$history %>% left_join(tables$profile.case %>% select(caseID, TDN), by = 'caseID')
  #     # todo: add extra columns and feed tdn and date before
  #     objects$sim_end <<- until
  #   }
  # },
  
  get.longest_station_path = function(full = T){
    get.case.path() %>% pull(transCount) %>% order %>% last -> lcid # longest caseid
    lcid = get.case.path()[lcid, 'caseID']
    if(full){hist = history} else {hist = history %>% filter(selected)}
    hist %>% filter(caseID == lcid) %>% pull(station) %>% unique
  },
  
  plotJourney = function(by_status = F, journey = T, incident = F, scheduled = F, simulation = F, group = F){
    
    if(by_status){
      get.traces() %>% pull(path) %>% strsplit('-') -> a
      if(length(a) > 0){
        lvls = a[[a %>% lapply(length) %>% unlist %>% order %>% tail(1)]]
        lvls = ifelse(lvls %in% c('START', 'END'), lvls, substr(lvls, 5, nchar(lvls))) %>% unique
        
        history %>% filter(selected) %>% 
          mutate(station = factor(status, levels = lvls)) %>% 
          plot_ly(type = 'scatter', x = ~startTime, y = ~status, color = ~caseID, mode = 'lines+markers')
      }  
      #   wide = get.status.case.time()
      #   
      #   wide %>% mutate(status = rownames(.)) %>% 
      #     mutate(station = ifelse(status %in% c('START', 'END'), status, substr(status, 5, nchar(status)))) %>% 
      #     mutate(station = factor(station, levels = lvls)) %>% arrange(station) %>% 
      #     viserPlot(x = 'station', y = colnames(wide) %>% as.list, plotter = 'plotly', type = 'combo', shape = 'line.point')
      # } else {cat('\n', 'Empty object! Nothing plotted.', '\n')}
    } 
    else {
      
      if (journey){
        history %>% filter(selected) %>% 
          select(caseID, startTime, station) %>% mutate(type = 'Journey') -> tbl
      } else {tbl = NULL}
      
      if(incident){
        get.incidents() %>%  
          select(time_from, st_from, st_to, incid, inctype) %>% 
          mutate(incid = paste0(incid, ' (', inctype, ')')) -> inc
        
        inc %>% select(caseID = incid, startTime = time_from, station = st_from) %>% 
          rbind(inc %>% select(caseID = incid, startTime = time_from, station = st_to)) %>% 
          arrange(caseID, startTime) %>% unique %>% mutate(type = 'Incident') %>% rbind(tbl) -> tbl
      }
      
      if(simulation & (!is.null(objects$sim))){
        objects$sim$history %>% filter(selected) %>% left_join(get.station_map(), by = 'status') %>% 
          # mutate(caseID = paste0('SIM (', caseID, ')')) %>% 
          select(caseID, startTime, station) %>% mutate(type = 'Simulation') %>% rbind(tbl) -> tbl
      }
      
      if(scheduled & (!is.null(objects$timetable))){
        # history %>% filter(status != 'START') %>% distinct(status, station) -> stmap
        objects$timetable$history %>% filter(selected) %>% 
          # left_join(stmap, by = 'status') %>% 
          # mutate(caseID = paste0('SIM (', caseID, ')')) %>% 
          select(caseID, startTime, station) %>% mutate(type = 'Scheduled') %>% rbind(tbl) -> tbl
      }
      
      tbl %>% 
        mutate(station = factor(station, levels = tables$profile.station %>% arrange(rank) %>% pull(station))) %>% 
        group_by(caseID) %>% 
        plot_ly(type = 'scatter', x = ~startTime, y = ~station, color = chif(group,~type, ~caseID), 
                symbol = ~type, mode = 'lines+markers')
    }
  },
  
  plotTimeline = function(){
    tbl = obj$history %>% filter(selected) %>% select(start = startTime, content = status, end = endTime, group = TDN) %>% mutate(id = 1:nrow(.), type = 'range')
    tblgrp = data.frame(id = unique(tbl$group)) %>% mutate(content = id)
    timevis::timevis(data = tbl, groups = tblgrp)
  }
  
  
))


