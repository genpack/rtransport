# transtools.R
# part1: simtools

# Not used anymore!
feedCaseStaticFeatures = function(obj, case_features, caseID_col = 'caseID'){
  case_features %>% nameColumns(list(caseID = caseID_col))
  obj$cases$features <- data.frame(caseID = obj$get.caseIDs(full = T), stringsAsFactors = F) %>% left_join(case_features, by = 'caseID')
}

buildTimeSeasonalityFeatures = function(obj){}

buildTimeStatusVolumeFeatures = function(obj){}

# Generates random values with given distribution subject to being greater than x0
# If you want to generate N random values with a distribution specified by cdf F(x) given that x > x0,
# F.inv(a*u + 1 - a)
# where a = 1 - F(x0) and u is random values with uniform distribution 
gen.random.cond = function(N = 1, x0 = -Inf, family = 'normal', ...){
  family = family %>% tolower %>% match.arg(family)
  a  = 1 - cdf(x0, family = family, ...)
  u  = runif(N)
  cdf.inv(a*u + 1 - a, family = family, ...)
}

# gen_conditional_random_values_verify = function(N = 1, x0 = -Inf, family = c('exponential', 'normal', 'gaussian'), ...){
#   family = family %>% tolower %>% match.arg(family)
#   params = list(...)
#   if(family == 'exponential'){
#     x0 %<>% max(0)
#     # Exponential distribution is memoryless, so random generated values should be simply added by x0
#     rv = x0 + rexp(N, rate = params$lambda)
#   } else if(family %in% c('gaussian', 'normal')){
#     a  = pnorm(x0, sd = params$sigma, mean = params$mu, lower.tail = F)
#     u  = runif(N)
#     # Verification: rv = params$sigma*sqrt(2)*pracma::erfinv(2*a*(u - 1) + 1) + params$mu
#     rv = qnorm(a*u + 1 - a, mean = params$mu, sd = params$sigma)
#   }
# }

# gets next event from scheduled without applying service alterations:
metrotrain_transition_classifier_v1 = function(histobj, input, ...){
  inp = input %>% left_join(histobj$objects$timetable$history %>% select(caseID, status, nextStatus), by = c('caseID', 'status'))
  wna = which(is.na(inp$nextStatus))
  
  if(length(wna > 0)){
    tdn_map = histobj$tables$profile.case %>% select(caseID, TDN)
    new = inp[wna,]
    transition_probabilities <- histobj$get.transition_probabilities.tdn() %>% filter(status %in% new$status)
    # transitions = histobj$history %>% 
    #   filter(status %in% new$status) %>% 
    #   left_join(tdn_map, by = "caseID") %>% 
    #   group_by(status, TDN, nextStatus) %>% 
    #   summarise(totalFreq = length(startTime)) %>% na.omit
    # 
    # transition_probabilities <- transitions %>% select(status, TDN, nextStatus, totalFreq) %>% arrange(status, TDN, nextStatus) %>% 
    #   group_by(status, TDN) %>% mutate(cum_freq = cumsum(totalFreq)) %>% 
    #   mutate(cum_prob = cum_freq/sum(totalFreq)) %>% ungroup() %>% 
    #   select(status, TDN, nextStatus, cum_prob)
    
    return(new %>% select(-nextStatus) %>% left_join(tdn_map, by = "caseID") %>% 
      left_join(transition_probabilities, by = c("status", "TDN")) %>% select(-TDN) %>% 
      rbind(inp[-wna, ] %>% mutate(cum_prob = 1)))
  } else {
    return(inp %>% mutate(cum_prob = 1))
  }
}

# gets next event from actual witout considering scheduled:
metrotrain_transition_classifier_v2 = function(histobj, input, ...){
  
  # inp = input %>% left_join(histobj$objects$timetable$history %>% select(caseID, status, nextStatus), by = c('caseID', 'status'))
  # Temporarily divine next status from history:
  inp = input %>% left_join(histobj$tables$future %>% select(caseID, status, nextStatus), by = c('caseID', 'status'))
  # Finished!
  
  wna = which(is.na(inp$nextStatus))
  
  if(length(wna > 0)){
    tdn_map = histobj$tables$profile.case %>% select(caseID, TDN)
    new = inp[wna,]
    transition_probabilities <- histobj$get.transition_probabilities.tdn() %>% filter(status %in% new$status)
    # transitions = histobj$history %>% 
    #   filter(status %in% new$status) %>% 
    #   left_join(tdn_map, by = "caseID") %>% 
    #   group_by(status, TDN, nextStatus) %>% 
    #   summarise(totalFreq = length(startTime)) %>% na.omit
    # 
    # transition_probabilities <- transitions %>% select(status, TDN, nextStatus, totalFreq) %>% arrange(status, TDN, nextStatus) %>% 
    #   group_by(status, TDN) %>% mutate(cum_freq = cumsum(totalFreq)) %>% 
    #   mutate(cum_prob = cum_freq/sum(totalFreq)) %>% ungroup() %>% 
    #   select(status, TDN, nextStatus, cum_prob)
    
    return(new %>% select(-nextStatus) %>% left_join(tdn_map, by = "caseID") %>% 
             left_join(transition_probabilities, by = c("status", "TDN")) %>% select(-TDN) %>% 
             rbind(inp[-wna, ] %>% mutate(cum_prob = 1)))
  } else {
    return(inp %>% mutate(cum_prob = 1))
  }
}

# runs monte-carlo simulation like markov-chain but altered services look at a different prob table
metrotrain_transition_classifier_v3 = function(histobj, input, ...){
  altered = histobj$tables$profile.case %>% filter(!is.na(alteration)) %>% pull(caseID)
  input %<>% left_join(histobj$get.tdn_map(), by = "caseID")
    
  inp_1 = input %>% filter(caseID %in% altered) %>% left_join(TRANSPORT.get.transition_probabilities.tdn.alt(histobj), by = c("status", "TDN"))
  nacse = inp_1$caseID[is.na(inp_1$cum_prob)]
  
  inp_1 %>% na.omit %>% 
    rbind(input %>% 
            filter((!caseID %in% altered) | (caseID %in% nacse)) %>% 
            left_join(histobj$get.transition_probabilities.tdn(), by = c("status", "TDN"))) -> inp_2
  
  # ctpd = inp_2$caseID %>% unique
  # inp_2 %<>% 
  #   left_join(histobj$get.station_map(), by = 'status') %>% 
  #   left_join(histobj$objects$timetable$history %>% 
  #               filter(caseID %in% ctpd) %>% 
  #               select(caseID, station, nxtStSch = nextStatus) %>% 
  #               distinct(caseID, station, .keep_all = T), by = c('caseID', 'station'))
  # 
  # inp_2[which(inp_2$nxtStSch == 'END'), 'nextStatus'] <- 'END'
  # inp_2 %>% select(- nxtStSch, -station)
  
  inp_2 %>% filter(nextStatus != 'END') %>% 
    left_join(histobj$get.station_map() %>% rename(nextStatus = status), by = 'nextStatus') %>% 
    left_join(histobj$objects$timetable$history %>% select(caseID, feasible = station) %>% distinct(caseID, feasible), by = 'caseID') %>% 
    filter(station == feasible) %>% select(-station, -feasible) %>% 
    rbind(inp_2 %>% filter(nextStatus == 'END'))
}

# runs monte-carlo simulation like markov-chain, altered services are treated the same way as normal services
metrotrain_transition_classifier_v4 = function(histobj, input, ...){
  input %<>% left_join(histobj$get.tdn_map(), by = "caseID")
  
  inp_2 = input %>% left_join(histobj$get.transition_probabilities.tdn(), by = c("status", "TDN"))

  inp_2 %>% filter(nextStatus != 'END') %>% 
    left_join(histobj$get.station_map() %>% rename(nextStatus = status), by = 'nextStatus') %>% 
    left_join(histobj$objects$timetable$history %>% select(caseID, feasible = station) %>% distinct(caseID, feasible), by = 'caseID') %>% 
    filter(station == feasible) %>% select(-station, -feasible) %>% 
    rbind(inp_2 %>% filter(nextStatus == 'END'))
}

# runs markov-chain monte-carlo simulation for generating both service alterations and next status
metrotrain_transition_classifier_v5 = function(histobj, input, ...){
  input %<>% left_join(histobj$get.tdn_map(), by = "caseID")
  
  inp_2 = input %>% left_join(TRANSPORT.get.service_alteration_probabilities.tdn(histobj), by = "TDN") %>% 
    group_by(caseID) %>% mutate(rand_var = runif(1)) %>%
    filter(rand_var < cum_prob) %>%
    filter(cum_prob == min(cum_prob)) %>%
    ungroup() %>% select(caseID, status, startTime, station, TDN, sd_flag, sa_flag, bl_flag, c_flag)

  inp_2 %>% filter(nextStatus != 'END') %>% 
    left_join(histobj$get.station_map() %>% rename(nextStatus = status), by = 'nextStatus') %>% 
    left_join(histobj$objects$timetable$history %>% select(caseID, feasible = station) %>% distinct(caseID, feasible), by = 'caseID') %>% 
    filter(station == feasible) %>% select(-station, -feasible) %>% 
    rbind(inp_2 %>% filter(nextStatus == 'END'))
}

# Prediction model: conditional mean, boosted for removing the bias of error:
metrotrain_transition_time_estimator_v2 = function(histobj, input, start_dt, ...) { 
  tdn_map = histobj$get.tdn_map()

  transition_durations_act <- TRANSPORT.get.transition_durations.tdn(histobj)
  
  inp = input %>% 
    left_join(tdn_map, by = "caseID") %>% 
    left_join(transition_durations_act, by = c("status", "nextStatus", "TDN"))
  
  tbd = which((inp$status == 'START') | (inp$nextStatus == 'END')) 
  inp$meanTime[tbd] <- 0.1
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = histobj$get.links()
    
    inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus) %>% 
      left_join(ad_base, by = c('status', 'nextStatus')) %>% pull(meanTime)
  }
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base <- histobj$objects$timetable$get.links() %>% 
      select(status, nextStatus, meanTime)
    
    inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus) %>% 
      left_join(ad_base, by = c('status', 'nextStatus')) %>% pull(meanTime)
  }

  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
     ad_base = TRANSPORT.get.transition_durations.tdn.station(histobj)
     
     inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus, TDN) %>%
       left_join(histobj$get.station_map(), by = 'status') %>% 
       left_join(histobj$get.station_map() %>% rename(nextStatus = status, nextStation =station), by = 'nextStatus') %>% 
       left_join(ad_base, by = c('station', 'nextStation', 'TDN')) %>% pull(meanTime)
  }

  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = TRANSPORT.get.transition_durations.station(histobj)
    
    inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus) %>%
      left_join(histobj$get.station_map(), by = 'status') %>% 
      left_join(histobj$get.station_map() %>% rename(nextStatus = status, nextStation =station), by = 'nextStatus') %>% 
      left_join(ad_base, by = c('station', 'nextStation')) %>% pull(meanTime)
  }
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = TRANSPORT.get.transition_durations.transit(histobj)
    inp$meanTime[wna] <- inp[wna,] %>% add_transit_column %>% 
      select(transit) %>%
      left_join(ad_base, by = 'transit') %>% pull(meanTime)
  }

  assert(sum(is.na(inp$meanTime)) == 0)
  # inp = inp[!is.na(inp$meanTime),]

  ## Correction: Add delays to transition durations for journeys impacted by incidents as primary journey:
  jwincs    = histobj$tables$profile.incident %>% pull(caseID) %>% unique
  # jwincs    = obj$tables$inc_full %>% filter(primary_flag == 1) %>% pull(caseID) %>% unique
  tocorr    = which((inp$caseID %in% jwincs) & (inp$status != 'START') & (inp$nextStatus != 'END'))
  if(length(tocorr) > 0){
    inp2 = inp[tocorr, ] %>%
      mutate(delay = 0) %>%
      left_join(obj$get.station_map(), by = 'status') %>%
      left_join(obj$get.station_map() %>% rename(nextStatus = status, nextStation = station), by = 'nextStatus') %>%
      add_transit_column %>%
      left_join(histobj$tables$profile.incident %>% distinct(caseID, .keep_all = T) %>% select(caseID, time_from, st_from, st_to, inctype), by = 'caseID') %>%
      mutate(incel = time_from > startTime, stmatch = (st_from == station) | (st_to == station), nxtstmatch  = (st_from == nextStation) | (st_to == nextStation))

    inp2$delay <- inp2 %>%
      left_join(TRANSPORT.get.transition_durations.inctype(histobj), by = c('incel', 'transit', 'stmatch', 'nxtstmatch', 'inctype')) %>% pull(avg_tdn_delay)

    wna = is.na(inp2$delay)
    inp2$delay[wna] <- inp2[wna,] %>%
      left_join(TRANSPORT.get.transition_durations.inc(histobj), by = c('incel', 'transit', 'stmatch', 'nxtstmatch')) %>% pull(avg_tdn_delay)
    
    inp[tocorr, 'meanTime'] <- inp[tocorr, 'meanTime'] + na2zero(inp2$delay)
  }
  
  inp %>% select(-TDN) %>% mutate(pred_duration = meanTime %>% abs)
}

# Prediction model: conditional mean for non-incident journeys, uses actual durations for incident-impacted journeys:
metrotrain_transition_time_estimator_PB = function (histobj, input, start_dt, ...) { 
  tdn_map = histobj$get.tdn_map()
  
  transition_durations_act <- TRANSPORT.get.transition_durations.tdn(histobj)
  
  inp = input %>% 
    left_join(tdn_map, by = "caseID") %>% 
    left_join(transition_durations_act, by = c("status", "nextStatus", "TDN"))
  
  tbd = which((inp$status == 'START') | (inp$nextStatus == 'END')) 
  inp$meanTime[tbd] <- 0.1
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = histobj$get.links()
    
    inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus) %>% 
      left_join(ad_base, by = c('status', 'nextStatus')) %>% pull(meanTime)
  }
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base <- histobj$objects$timetable$get.links() %>% 
      select(status, nextStatus, meanTime)
    
    inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus) %>% 
      left_join(ad_base, by = c('status', 'nextStatus')) %>% pull(meanTime)
  }
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = TRANSPORT.get.transition_durations.tdn.station(histobj)
    
    inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus, TDN) %>%
      left_join(histobj$get.station_map(), by = 'status') %>% 
      left_join(histobj$get.station_map() %>% rename(nextStatus = status, nextStation =station), by = 'nextStatus') %>% 
      left_join(ad_base, by = c('station', 'nextStation', 'TDN')) %>% pull(meanTime)
  }
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = TRANSPORT.get.transition_durations.station(histobj)
    
    inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus) %>%
      left_join(histobj$get.station_map(), by = 'status') %>% 
      left_join(histobj$get.station_map() %>% rename(nextStatus = status, nextStation =station), by = 'nextStatus') %>% 
      left_join(ad_base, by = c('station', 'nextStation')) %>% pull(meanTime)
  }
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = TRANSPORT.get.transition_durations.transit(histobj)
    inp$meanTime[wna] <- inp[wna,] %>% add_transit_column %>% 
      select(transit) %>%
      left_join(ad_base, by = 'transit') %>% pull(meanTime)
  }
  
  assert(sum(is.na(inp$meanTime)) == 0)
  # inp = inp[!is.na(inp$meanTime),]
  

  inp %<>% select(-TDN) %>% mutate(pred_duration = meanTime %>% abs)
  
  # Temporarily divine transition duration from history for journeys being impacted by an incident as primary service:
  jwincs   = histobj$tables$profile.incident %>% pull(caseID) %>% unique
  todivine = which(input$caseID %in% jwincs) %>% c(wna)
  
  inp$pred_duration[todivine] <- input[todivine, ] %>% 
    left_join(histobj$tables$future %>% select(caseID, status, nextStatus, duration), by = c('caseID', 'status', 'nextStatus')) %>% 
    pull(duration)
  
  wna = is.na(inp$pred_duration) %>% which
  if(length(wna) > 0){
    inp$pred_duration[wna] <- inp$meanTime[wna] %>% abs
  }
  wna = is.na(inp$pred_duration) %>% which
  assert(length(wna) == 0)
  # Finished!
  
  return(inp)
}

metrotrain_next_event_generator = function(tracking, histobj, ...){
  gen_next_events(input = tracking, histobj = histobj, ...)
  ## todo: send to corrector to take care of service alterations
}
  
metrotrain_transition_time_generator = function(input, histobj, new_events, start_dt, ...){
  gen_transition_times(histobj = histobj, input = input, start_dt = start_dt, new_events = new_events, ...)
}

find_clashes = function(obj, lines, i){
  ww = lines %>% select(x1, y1, x2, y2) %>% lines_meet(i) %>% which
  if(length(ww) > 0){
    jc = TRANSPORT.get.journey_clash_aggregates(obj)
    data.frame(index = ww, 
               station_1 = lines$station[i], platform_1 = lines$platform[i], 
               nextStation_1 = lines$nextStation[i], nextPlatform_1 = lines$nextPlatform[i],
               station_2 = lines$station[ww], platform_2 = lines$platform[ww], 
               nextStation_2 = lines$nextStation[ww], nextPlatform_2 = lines$nextPlatform[ww]) %>% 
      anti_join(jc, by = c('station_1', 'platform_1', 'nextStation_1', 'nextPlatform_1',
                           'station_2', 'platform_2', 'nextStation_2', 'nextPlatform_2')) %>% pull(index) -> clashes
  } else {clashes = integer()}
  return(clashes)
}  

# Correction of Next Transition to apply headway rules:
correct_transition_times.old = function(histobj, tracking, final_events){
  ### For Test:

  tracking %<>% arrange(desc(startTime))
  station_map    = histobj$get.station_map()
  platform_map   = histobj$get.platform_map()
  station_levels = histobj$tables$profile.station %>% arrange(rank) %>% pull(station)
  
  tracking %>% select(caseID, status, startTime, nextStatus, endTime = nxtTrTime) %>% 
    rbind(final_events %>% select(caseID, status, startTime, nextStatus, endTime = nxtTrTime)) %>% 
    rbind(histobj$history %>% select(caseID, status, startTime, nextStatus, endTime) %>% 
            filter(endTime > min(tracking$startTime), startTime < max(tracking$startTime))) %>% 
    # filter(status != 'START' & nextStatus!= 'END') %>% 
    left_join(station_map, by = 'status') %>% left_join(station_map %>% rename(nextStatus = status, nextStation = station), by = 'nextStatus') %>% 
    mutate(x1 = difftime(startTime, histobj$modelStart, units = 'mins') %>% as.numeric, y1 = factor(station, levels = station_levels) %>% as.integer, x2 = difftime(endTime, histobj$modelStart, units = 'mins') %>% as.numeric, y2 = factor(nextStation, levels = station_levels) %>% as.integer) %>% 
    left_join(platform_map, by = 'status') %>% 
    left_join(platform_map %>% select(nextStatus = status, nextPlatform = platform), by = 'nextStatus') %>% 
    left_join(histobj$get.tdn_map(), by = 'caseID') -> lines
  
  maxrank = max(lines$y1, lines$y2, na.rm = T)

  wstart = which(lines$status == 'START')
  wend   = which(lines$nextStatus == 'END')
  lines$y1[wstart] <- lines$y1[wstart] - 0.5
  lines$y2[wend]   <- lines$y2[wend] + 0.5
  
  # tc = which(tracking$status != 'START' & tracking$nextStatus != 'END')
  tc = sequence(nrow(lines))
  
  if(nrow(lines) > 0){
    sltc = length(tc) %>% sequence

    for(i in sltc){
      lines$x1[i] = lines$x1[i] - 2
      clashes = find_clashes(histobj, lines, i)
      if(length(clashes) > 0){
          options = histobj$get.transition_probabilities.tdn() %>% 
            filter(status == lines$status[i], TDN == lines$TDN[i], nextStatus != lines$nextStatus[i]) %>% 
            select(nextStatus, cum_prob) %>% 
            rbind(data.frame(nextStatus = lines$status[i], cum_prob = 0)) %>% 
            arrange(desc(cum_prob)) %>% left_join(histobj$get.station_map() %>% rename(nextStatus = status), by = 'nextStatus') %>% 
            left_join(histobj$tables$profile.station %>% select(station, rank), by = 'station')
          
          while((length(clashes) > 0) & (nrow(options) > 0)){
            lines$nextStatus[i]        <- options$nextStatus[1]
            lines$y2[i]                <- options$rank[1]
            tracking$nextStatus[tc[i]] <- options$nextStatus[1]
            
            tracking$meanTime[tc[i]]   <- lines[i,] %>% select(status, nextStatus) %>% 
              left_join(histobj$get.links(), by = c('status', 'nextStatus')) %>% pull(meanTime)
            if(is.na(tracking$meanTime[tc[i]])){tracking$meanTime[tc[i]] <- 20}
            tracking$nxtTrTime[tc[i]]  <- tracking$startTime[tc[i]] + tracking$meanTime[tc[i]]
            lines$x2[i]                <- lines$x1[i] + 2 + tracking$meanTime[tc[i]]/60
            
            options = options[-1,]
            if(nrow(options) > 0) clashes = find_clashes(histobj, lines, i)
          }
      }
      lines$x1[i] = lines$x1[i] + 2
    }
  }
  return(tracking)
}

correct_transition_times = function(histobj, tracking, final_events){
  if(!is.empty(histobj$objects$future_elog)){
    occupied = histobj$objects$future_elog %>% select(status, startTime) %>% 
      left_join(histobj$get.station_map(), by = 'status') %>% 
      left_join(histobj$get.platform_map(), by = 'status') %>% select(station, platform, time = startTime) %>% 
      rbind(histobj$objects$future_elog %>% select(status = nextStatus, startTime = nxtTrTime) %>% 
            left_join(histobj$get.station_map(), by = 'status') %>% 
            left_join(histobj$get.platform_map(), by = 'status') %>% select(station, platform, time = startTime)) %>% 
      group_by(station, platform) %>% summarise(mintime = max(time)) %>% na.omit
      
    tracking %>% select(status = nextStatus, nxtTrTime) %>% 
      left_join(histobj$get.station_map(), by = 'status') %>% 
      left_join(histobj$get.platform_map(), by = 'status') %>% 
      left_join(occupied, by = c('station', 'platform')) -> evaluate
    
    w = which(evaluate$nxtTrTime < evaluate$mintime) 
    if(length(w) > 0){
      tracking$nextStatus[w] = tracking$status[w]
      tracking$nxtTrTime[w]  = tracking$startTime[w] + difftime(evaluate$mintime[w], evaluate$nxtTrTime[w], units = 'secs')
    }
  }
  return(tracking)
}

lines_meet.old = function(lines, ln = 1){
  lines %<>% as.matrix
  x1    = lines[ln ,1]
  y1    = lines[ln ,2]
  x2    = lines[ln ,3]
  y2    = lines[ln ,4]
  x3    = lines[-ln ,1]
  y3    = lines[-ln ,2]
  x4    = lines[-ln ,3]
  y4    = lines[-ln ,4]
  a1    = y2 - y1
  a2    = y4 - y3
  b1    = x1 - x2
  b2    = x3 - x4
  u1    = a1*x1 + b1*y1
  u2    = a2*x3 + b2*y3
  den   = a1*b2 - b1*a2
  xc    = (b2*u1 - b1*u2)/den
  yc    = (a1*u2 - a2*u1)/den
  
  assert(abs((y2 - y1)*(xc - x1) - (x2 - x1)*(yc - y1)) %>% sum(na.rm = T) < 0.000001)
  assert(abs((y4 - y3)*(xc - x3) - (x4 - x3)*(yc - y3)) %>% sum(na.rm = T) < 0.000001)
  
  xh = max(x2, x1) 
  xl = min(x2, x1)
  yh = max(y2, y1) 
  yl = min(y2, y1)
  fc = (xc < xh) & (xc > xl) & (yc < yh) & (yc > yl)
  x_max = lines[-ln,c(1,3)] %>% apply(1, max)
  x_min = lines[-ln,c(1,3)] %>% apply(1, min)
  y_max = lines[-ln,c(2,4)] %>% apply(1, max)
  y_min = lines[-ln,c(2,4)] %>% apply(1, min)
  
  (xc < xh) & (xc > xl) & (yc < yh) & (yc > yl) & (xc < x_max) & (xc > x_min) & (yc < y_max) & (yc > y_min) %>% na2zero
  
}

lines_meet = function(lines, ln = 1){
  lines %<>% as.matrix
  out = rep(FALSE, nrow(lines))
  out[ln] <- NA
  
  fx    = lines[, 1] < lines[, 3]
  fy    = lines[, 2] < lines[, 4]
  xl    = ifelse(fx, lines[, 1], lines[, 3]) 
  xh    = ifelse(fx, lines[, 3], lines[, 1]) 
  yl    = ifelse(fy, lines[, 2], lines[, 4]) 
  yh    = ifelse(fy, lines[, 4], lines[, 2]) 
  
  x1    = lines[ln, 1]
  y1    = lines[ln, 2]
  x2    = lines[ln, 3]
  y2    = lines[ln, 4]
  
  # x1    = xl[ln]
  # y1    = yl[ln]
  # x2    = xh[ln]
  # y2    = yh[ln]

  tk    = which((xh > xl[ln]) & (xl < xh[ln]) & (yh > yl[ln]) & (yl < yh[ln]))
  
  if(length(tk > 0)){
    # x3    = xl[tk]
    # y3    = yl[tk]
    # x4    = xh[tk]
    # y4    = yh[tk]
    
    x3    = lines[tk, 1]
    y3    = lines[tk, 2]
    x4    = lines[tk, 3]
    y4    = lines[tk, 4]
    
    a1    = y2 - y1
    a2    = y4 - y3
    b1    = x1 - x2
    b2    = x3 - x4
    u1    = a1*x1 + b1*y1
    u2    = a2*x3 + b2*y3
    den   = a1*b2 - b1*a2
    xc    = (b2*u1 - b1*u2)/den
    yc    = (a1*u2 - a2*u1)/den
    
    # assert(abs((y2 - y1)*(xc - x1) - (x2 - x1)*(yc - y1)) %>% sum(na.rm = T) < 0.000001)
    # assert(abs((y4 - y3)*(xc - x3) - (x4 - x3)*(yc - y3)) %>% sum(na.rm = T) < 0.000001)
    
    # out[tk] <- (xc < x2) & (xc > x1) & (yc < y2) & (yc > y1) & (xc < x4) & (xc > x3) & (yc < y4) & (yc > y3)
    out[tk] <- (xc < xh[ln]) & (xc > xl[ln]) & (yc < yh[ln]) & (yc > yl[ln]) & (xc < xh[tk]) & (xc > xl[tk]) & (yc < yh[tk]) & (yc > yl[tk])
  }
  return(out)
}

plot_lines = function(lines){
  p1 = lines[, c(1,2)]
  p2 = lines[, c(3,4)]
  sq = 1:nrow(p1)
  p1 %<>% as.data.frame %>% {names(.) <- c('x','y');.} %<>% mutate(rank = 2*sq - 1, cl = sq)
  p2 %<>% as.data.frame %>% {names(.) <- c('x','y');.} %<>% mutate(rank = 2*sq, cl = sq)
  rbind(p1,p2) %>% mutate(cl = as.factor(cl)) %>% arrange(rank) %>% group_by(cl) %>% 
    plot_ly(type = 'scatter', x = ~x, y = ~y, color = ~cl, mode = 'lines+markers')
}

# p1 = data.frame(x = rnorm(12), y = rnorm(12))
# p2 = data.frame(x = rnorm(12), y = rnorm(12))
# lines_meet(cbind(p1,p2), 2)
# plot_lines(p1, p2)

interval = function(a, b, inv = F){
  if(inv){inflg = c(T, F, T)} else {inflg = c(F, T, F)}
  data.frame(value = c(-Inf, min(a,b), max(a,b)), inflag = inflg)
}

interval.intersect = function(I1, I2){
  I1 %>% full_join(I2, by = 'value') %>% arrange(value) -> mix
  for(i in 1:nrow(mix)){
    if(is.na(mix[i, 'inflag.x'])){
      mix[i, 'inflag.x'] <- mix[i-1, 'inflag.x']
    }
    if(is.na(mix[i, 'inflag.y'])){
      mix[i, 'inflag.y'] <- mix[i-1, 'inflag.y']
    }
  }
  mix %>% 
    mutate(inflag = inflag.x & inflag.y) %>% mutate(prevFlag = c(!first(inflag), inflag[-length(inflag)])) %>% 
    filter(inflag != prevFlag) %>% select(value, inflag)
}

interval.union = function(I1, I2){
  I1 %>% full_join(I2, by = 'value') %>% arrange(value) -> mix
  for(i in 1:nrow(mix)){
    if(is.na(mix[i, 'inflag.x'])){
      mix[i, 'inflag.x'] <- mix[i-1, 'inflag.x']
    }
    if(is.na(mix[i, 'inflag.y'])){
      mix[i, 'inflag.y'] <- mix[i-1, 'inflag.y']
    }
  }
  mix %>% mutate(inflag = inflag.x | inflag.y) %>% mutate(nextFlag = c(inflag[-1], !last(inflag))) %>% filter(inflag != nextFlag) %>% select(value, inflag)
}

# checks if any two transitions in the transition history (eventlog), encounter each other and returns a list of encounters
TRANSPORT.get.journey_clashes = function(obj, show_progress = T){
  if(is.null(obj$tables$journey_clashes)){
    t0    = obj$modelStart
    stmap = obj$get.station_map()
    plmap = obj$get.platform_map()
    lvls  = obj$tables$profile.station %>% arrange(rank) %>% pull(station)
    lines <- obj$history %>% mutate(row = sequence(nrow(.))) %>% filter(selected) %>% select(row, caseID, startDate, status, startTime, nextStatus, endTime) %>% 
      filter(status != 'START' & nextStatus!= 'END') %>% 
      left_join(stmap, by = 'status') %>% left_join(stmap %>% rename(nextStatus = status, nextStation = station), by = 'nextStatus') %>% 
      left_join(plmap, by = 'status') %>% 
      left_join(plmap %>% select(nextStatus = status, nextPlatform = platform), by = 'nextStatus') %>% 
      mutate(x1 = difftime(startTime, t0, units = 'mins') %>% as.numeric, y1 = factor(station, levels = lvls) %>% as.integer, x2 = difftime(endTime, t0, units = 'mins') %>% as.numeric, y2 = factor(nextStation, levels = lvls) %>% as.integer)
    
    enc = NULL
    done = c()
    cids = lines$caseID %>% unique
    nnnn = length(cids)
    if(show_progress){pb = txtProgressBar(min = 1, max = nnnn, style = 3)}
    for(i in sequence(nnnn)){
      # cat(cdmp$caseID[i], '\n')
      
      done     = c(done, cids[i])
      lines_1  = lines %>% filter(caseID == cids[i])
      min_time = min(lines_1$startTime)
      max_time = max(lines_1$endTime)
      # lines_2 = lines %>% filter(!(caseID %in% done), startDate == cdmp$startDate[i])
      lines_2 = lines %>% filter(!(caseID %in% done), startTime < max_time, endTime > min_time)
      # lines_1$x1 = lines_1$x1 - 2 # 2 minutes headway
      for(j in sequence(nrow(lines_1))){
        # cat(j, ' ')
        l = rbind(lines_1[j,], lines_2)
        w = lines_meet(l[, c('x1', 'y1', 'x2', 'y2')], 1) %>% which
        if(length(w) > 0){
          enc %<>% rbind(cbind(l[1,] %>% {colnames(.)<-colnames(.) %>% paste(1, sep = '_');.}, l[w,] %>% {colnames(.)<-colnames(.) %>% paste(2, sep = '_');.}))
        }
        
        # # cat(j, ' ')
        # l = rbind(lines_1[j,], lines_2)
        # w = lines_meet(l[, c('x1', 'y1', 'x2', 'y2')], 1) %>% which
        # if(length(w) > 0){
        #   enc %<>% rbind(cbind(l[1,], lines_2[w-1, ]))
        # }
      }
      if(show_progress){setTxtProgressBar(pb, i)}
    }
    obj$tables$journey_clashes = enc
  }
  return(obj$tables$journey_clashes)
}

TRANSPORT.get.journey_clash_aggregates = function(obj){
  if(is.null(obj$tables$journey_clash_aggregates)){
    # obj$tables$journey_clashes <- read.csv('clashes.csv') %>% select(-X)
    cn = colnames(TRANSPORT.get.journey_clashes(obj))
    c2 = obj$tables$journey_clashes
    colnames(c2) <- cn %>% 
      gsub(pattern = '_1', replacement = '_0') %>% 
      gsub(pattern = '_2', replacement = '_1') %>% 
      gsub(pattern = '_0', replacement = '_2')
    
    rbind(obj$tables$journey_clashes, c2[, cn]) %>% 
      group_by(station_1, platform_1, nextStation_1, nextPlatform_1, station_2, platform_2, nextStation_2, nextPlatform_2) %>% 
      summarise(cnt = length(caseID_1)) %>% arrange(desc(cnt)) -> obj$tables$journey_clash_aggregates
  }
  
  return(obj$tables$journey_clash_aggregates)
}

# returns which incidents had clashed with journeys
TRANSPORT.get.incident_clashes = function(obj){
  incident_duration = 60
  stmap = obj$get.station_map()
  lvls  = obj$tables$profile.station %>% arrange(rank) %>% pull(station)
  lines_inc <- obj$get.incidents() %>%  
    select(caseID = incid, inctype, incident_date, startTime = time_from, station = st_from, nextStation = st_to) %>% 
    mutate(x1 = difftime(startTime, obj$modelStart, units = 'mins') %>% as.numeric, y1 = factor(station, levels = lvls) %>% as.integer, x2 = difftime(startTime + incident_duration, obj$modelStart, units = 'mins') %>% as.numeric, y2 = factor(nextStation, levels = lvls) %>% as.integer)
  lines <- obj$history %>% mutate(row = sequence(nrow(.))) %>% filter(selected) %>% select(row, caseID, startDate, status, startTime, nextStatus, endTime) %>% 
    filter(status != 'START' & nextStatus!= 'END') %>% 
    left_join(stmap, by = 'status') %>% left_join(stmap %>% rename(nextStatus = status, nextStation = station), by = 'nextStatus') %>% 
    mutate(x1 = difftime(startTime, obj$modelStart, units = 'mins') %>% as.numeric, y1 = factor(station, levels = lvls) %>% as.integer, x2 = difftime(endTime, obj$modelStart, units = 'mins') %>% as.numeric, y2 = factor(nextStation, levels = lvls) %>% as.integer)
  for(i in sequence(nrow(lines_inc))){
    lines_i = lines %>% filter(startDate == lines_inc$incident_date[i])
    lines_inc[i, c('x1', 'y1', 'x2', 'y2')] %>% 
      rbind(lines_i %>% select(x1, y1, x2, y2)) %>% 
      lines_meet(1) %>% which -> w
    lines_inc$clashes[i] <- lines_i[w-1, 'row'] %>% paste(collapse = ',')
    lines_inc$nclash[i]  <- length(w)
  }
  return(lines_inc)
}

add_transit_column = function(events){
  events %>% mutate(transit = 0) %>% {.$transit[.$status %>% grep(pattern = 'Dep.')] <- 1;.$transit[.$status == 'START'] <- 2;.} 
}

add_swp_column = function(events){
  events %>% mutate(swp = substr(status, 1, 7)) 
}

TRANSPORT.get.sim_comparison = function(obj){
  if(is.null(obj$report$comparison)){
    obj$report$comparison <- obj$history %>% add_transit_column %>% 
      select(caseID, status, nextStatus, actTime = startTime, actDur = duration, transit) %>% 
      inner_join(obj$objects$timetable$history %>% select(caseID, status, nextStatus, schTime = startTime, schDur = duration), by = c('caseID', 'status', 'nextStatus')) %>% 
      inner_join(obj$objects$sim$history %>% select(caseID, status, nextStatus, simTime = startTime, simDur = duration), by = c('caseID', 'status', 'nextStatus')) %>% 
      mutate(time_error = difftime(simTime, actTime, units = 'secs') %>% as.numeric, 
             dur_error  = simDur - actDur,
             delay_act  = difftime(actTime, schTime, units = 'secs') %>% as.numeric, 
             delay_sim  = difftime(simTime, schTime, units = 'secs') %>% as.numeric) %>% 
      mutate(time_abser = abs(time_error), dur_abser = abs(dur_error))
  }
  return(obj$report$comparison)  
}

TRANSPORT.get.sim_performance = function(obj, threshold = 300){
  # How many delays have you been able to predict?
  TRANSPORT.get.sim_comparison(obj) %>% 
    filter(transit == 0) %>% 
    mutate(delayed_act = delay_act > threshold, delayed_sim = delay_sim > threshold) %>% select(delayed_act, delayed_sim) %>% 
    scorer(prediction_col = 'delayed_sim', actual_col = 'delayed_act')
}

TRANSPORT.get.tdns = function(obj){
  if(is.null(obj$tables$profile.tdn)){
    obj$objects$timetable$history %>% group_by(startDate, TDN) %>% summarise(startTime = min(startTime)) %>% 
      mutate(mod = difftime(startTime, as.POSIXct(startDate), units = 'mins')) %>% group_by(TDN) %>% 
      summarise(avgmod_sch = mean(mod)) %>% arrange(avgmod_sch) %>% mutate(rank = sequence(n())) -> sch
    
    obj$history %>% filter(nextStatus != 'END') %>% 
      group_by(startDate, TDN, direction) %>% summarise(startTime = min(startTime)) %>% na.omit %>% 
      mutate(mod = difftime(startTime, as.POSIXct(startDate), units = 'mins')) %>% group_by(TDN, direction) %>% 
      summarise(avgmod_act = mean(mod)) %>% left_join(sch, by = 'TDN') %>% arrange(rank) -> obj$tables$profile.tdn
  }
  return(obj$tables$profile.tdn)
}

correct_events = function(obj, events, headway_seconds = 120){
  events %>% select(caseID, status, nextStatus, startTime, endTime) %>% add_transit_column %>% 
    left_join(obj$get.tdn_map(), by = 'caseID') %>% 
    left_join(obj$get.station_map(), by = 'status') %>% 
    left_join(obj$get.platform_map(), by = 'status') %>% 
    mutate(location = paste(station, platform, sep = '.')) %>% arrange(startTime) %>% as.data.frame -> events
  
  obj$history %>% filter(startTime < obj$ctime) %>% 
    mutate(location = paste(station, platform, sep = '.')) %>% 
    select(caseID, startTime, location) %>% arrange(startTime) %>% 
    group_by(location) %>% summarise(mintime = last(startTime) + headway_seconds, service = last(caseID)) %>% ungroup %>% 
    column2Rownames('location') -> occupied
  
  mintime = min(obj$history$startTime)
  occupied[paste(obj$history$station, obj$history$platform, sep = '.') %>% c(events$location) %>% unique %>% setdiff(rownames(occupied)), 'mintime'] <- mintime

  ne = nrow(events)
  pb = txtProgressBar(min = 1, max = ne, style = 3)
  i  = 1
  while(i <= ne){
    ignore = T
    setTxtProgressBar(pb, i)
    while(events$status[i] == 'START'){i = i + 1}
    if((events$startTime[i] < occupied[events$location[i], 'mintime']) & (occupied[events$location[i], 'service'] != events$caseID[i])){
      w = which((events$caseID == events$caseID[i]) & (events$endTime >=  events$startTime[i]))
      ignore = (events[w[1], 'status'] == 'START') | (events[i,'nextStatus'] == 'END')
      if(!ignore){
        opts = obj$history %>% 
          filter(status == events[w[1], 'status'], TDN == events[w[1], 'TDN'], nextStatus != events[w[1], 'nextStatus']) %>% 
          mutate(location = paste(station, platform, sep = '.')) %>% distinct(status, nextStatus, TDN, location)
        
        # opts = obj$get.transition_probabilities.tdn() %>% 
        #   filter(status == events[w[1], 'status'], TDN == events[w[1], 'TDN'], nextStatus != events[w[1], 'nextStatus']) %>% 
        #   left_join(obj$get.station_map() %>% rename(nextStatus = status), by = 'nextStatus') %>% 
        #   left_join(obj$get.platform_map() %>% rename(nextStatus = status), by = 'nextStatus') %>%
        #   mutate(location = paste(station, platform, sep = '.'))
        
        shifttime = T
        while((nrow(opts) > 0) & shifttime){
          obj$get.transition_probabilities.tdn() %>% 
            filter(status == opts$nextStatus[1], TDN == events[w[2], 'TDN'], nextStatus == events[w[2], 'nextStatus']) -> possible
          
          feasible = (nrow(possible) > 0) & (occupied[opts$location[1], 'mintime'] < events[w[1], 'startTime'])
          if(feasible > 0){
            events[w[1], 'nextStatus'] <- opts$nextStatus[1]
            events[w[2], 'status']     <- opts$nextStatus[1]
            events[w[2], 'location']   <- opts$location[1]
            events[w[2], 'station']    <- opts$station[1]
            events[w[2], 'platform']   <- opts$platform[1]
            
            shifttime = F
          }
          opts = opts[-1,]
        }
        if(shifttime) {
          timeshift <- difftime(occupied[events$location[i], 'mintime'], events$startTime[i], units = 'secs')
          w1 = chif(i == w[1], w, w[-1]); assert(i %in% w1)
          events[w1, 'startTime'] <- events$startTime[w1] + timeshift
          events[w, 'endTime']    <- events$endTime[w] + timeshift
          events %<>% arrange(startTime)
        }
        #i = w[1]
        #occupied_correct = events[sequence(i-1), ] %>% group_by(location) %>% summarise(mintime = last(startTime) + 120, service = last(caseID)) %>% ungroup
        #occupied[occupied_correct$location, 'mintime'] <- occupied_correct$mintime
      }
    }
    if(ignore){
      occupied[events$location[i], 'mintime'] <- chif(events$transit[i] == 0, events$endTime[i], events$startTime[i]) + headway_seconds
      occupied[events$location[i], 'service'] <- events$caseID[i]
      i = i + 1
    }
  }
  return(events)
}

headway_violations = function(obj, events = NULL){
  if(is.null(events)){events <- obj$history %>% filter(startTime > obj$ctime)}
  events %>% select(caseID, status, nextStatus, startTime, endTime) %>% 
    left_join(obj$get.tdn_map(), by = 'caseID') %>% 
    left_join(obj$get.station_map(), by = 'status') %>% 
    left_join(obj$get.platform_map(), by = 'status') %>% 
    mutate(location = paste(station, platform, sep = '.')) %>% 
    mutate(violation = NA, occupying_jid = NA, occupied_until = startTime, headway_secs = NA, headway_time = startTime) %>% arrange(startTime) -> events
  
  obj$history %>% filter(startTime < min(events$startTime)) %>% 
    mutate(location = paste(station, platform, sep = '.')) %>% 
    select(caseID, startTime, location, station) %>% 
    left_join(obj$tables$profile.station %>% select(station, headway)) %>% 
    mutate(startTime = startTime + headway) %>% arrange(startTime) %>% 
    group_by(location, station) %>% summarise(mintime = last(startTime), service = last(caseID)) %>% ungroup %>% 
    column2Rownames('location') -> occupied

  mintime = min(obj$history$startTime)
  occupied[paste(obj$history$station %>% na.omit, obj$history$platform %>% na.omit, sep = '.') %>% c(events$location) %>% unique %>% setdiff(c(rownames(occupied), 'NA.NA')), 'mintime'] <- mintime
  occupied$station = rownames(occupied) %>% substr(1,3)
  occupied$headway = occupied %>% left_join(obj$tables$profile.station %>% select(station, headway), by = 'station') %>% pull(headway)
  occupied = occupied[!is.na(occupied$headway),]
  
  ne = nrow(events)
  pb = txtProgressBar(min = 1, max = ne, style = 3)
  for(i in sequence(ne)){
    setTxtProgressBar(pb, i)
    if((events$status[i] != 'START') & (events$nextStatus[i] != 'END')){
      if((events$startTime[i] < occupied[events$location[i], 'mintime']) & (occupied[events$location[i], 'service'] != events$caseID[i])){
        w = which((events$caseID == events$caseID[i]) & (events$endTime >  events$startTime[i] - 0.01))
        if(events[w[1], 'status'] != 'START'){
          # events$description[i]  <- 'Location:' %>% paste(events$location[i], 'is occupied by', occupied[events$location[i], 'service'], 'until:', occupied[events$location[i], 'mintime'])
          events$violation[i]      <- difftime(occupied[events$location[i], 'mintime'], events$startTime[i], units = 'secs')
          events$occupying_jid[i]  <- occupied[events$location[i], 'service']
          events$occupied_until[i] <- occupied[events$location[i], 'mintime'] - occupied[events$location[i], 'headway']
          events$headway_secs[i]   <- occupied[events$location[i], 'headway']
          events$headway_time[i]   <- occupied[events$location[i], 'mintime']
        }
      }  
      occupied[events$location[i], 'mintime'] <- events$startTime[i] + occupied[events$location[i], 'headway']
      occupied[events$location[i], 'service'] <- events$caseID[i]
    }
  }
  return(events %>% filter(!is.na(violation)))
}

TRANSPORT.get.station_transition_times = function(obj){
  if(is.null(obj$tables$station_transition_times)){
    obj$history %>% group_by(station, nextStation) %>% summarise(meanTime = mean(duration, na.rm = T)) -> obj$tables$station_transition_times
  }
  return(obj$tables$station_transition_times)
}

TRANSPORT.get.transition_durations.tdn = function(obj){
  if(!is.null(obj$tables[['transition_durations.tdn']])) obj$report[['transition_durations.tdn']] <- obj$tables[['transition_durations.tdn']]
  
  if(is.null(obj$report$transition_durations.tdn)){
    #obj$tables$profile.case$impacted = (obj$tables$profile.case$caseID %in% (obj$tables$incident_impacts$jid %U% obj$tables$profile.incident$caseID))
    obj$history %>% filter(selected) %>% 
      filter(!(caseID %in% (obj$tables$incident_impacts$jid %U% obj$tables$profile.incident$caseID))) %>% 
      group_by(status, nextStatus, TDN) %>% 
      summarise(meanTime = mean(duration, na.rm = T), sdTime = sd(duration, na.rm = T)) %>% 
      select(status, nextStatus, TDN, meanTime, sdTime) %>% 
      arrange(status, nextStatus, TDN) %>% na2zero -> obj$report[['transition_durations.tdn']]
  }
  return(obj$report[['transition_durations.tdn']])
}

TRANSPORT.get.transition_durations.inctype = function(obj){
  if(is.null(obj$report$transition_durations.inctype)){
    TRANSPORT.get.transitions(obj) %>% filter(!is.na(inctype)) %>% 
      mutate(incel = inc_elapsed > 0) %>% 
      left_join(obj$get.tdn_map(), by = 'caseID') %>% 
      #left_join(obj$get.station_map(), by = 'status') %>% 
      #left_join(obj$get.station_map() %>% rename(nextStatus = status, nextStation = station), by = 'nextStatus') %>% 
      left_join(TRANSPORT.get.transition_durations.tdn(obj), by = c('status', 'nextStatus', 'TDN')) %>% 
      mutate(tdn_delay   = duration - meanTime) %>% 
      mutate(stmatch     = (st_from == station) | (st_to == station)) %>% 
      mutate(nxtstmatch  = (st_from == nextStation) | (st_to == nextStation)) %>% 
      group_by(transit, incel, stmatch, nxtstmatch, inctype) %>% 
      summarise(avg_tdn_delay = mean(tdn_delay, na.rm = T), sd_tdn_delay = sd(tdn_delay, na.rm = T), cnt = length(tdn_delay)) %>% 
      mutate(tstats = sqrt(cnt)*avg_tdn_delay/sd_tdn_delay) %>% 
      mutate(pvalue = round(1 - pt(tstats, df = cnt - 1),3)) %>% 
      filter(pvalue < 0.05) -> obj$report$transition_durations.inctype
  }
  return(obj$report$transition_durations.inctype)
}

TRANSPORT.get.transition_durations.inc = function(obj){
  if(is.null(obj$report[['transition_durations.inc']])){
    #TRANSPORT.add.case_features.incident(obj)
    #TRANSPORT.add.transition_features.case(obj)
    TRANSPORT.get.transitions(obj) %>% filter(!is.na(inctype)) %>% 
      mutate(incel = inc_elapsed > 0) %>% 
      left_join(obj$get.tdn_map(), by = 'caseID') %>% 
      # left_join(obj$get.station_map(), by = 'status') %>% 
      # left_join(obj$get.station_map() %>% rename(nextStatus = status, nextStation = station), by = 'nextStatus') %>% 
      left_join(TRANSPORT.get.transition_durations.tdn(obj), by = c('status', 'nextStatus', 'TDN')) %>% 
      mutate(tdn_delay   = duration - meanTime) %>% 
      mutate(stmatch     = (st_from == station) | (st_to == station)) %>% 
      mutate(nxtstmatch  = (st_from == nextStation) | (st_to == nextStation)) %>% 
      group_by(transit, incel, stmatch, nxtstmatch) %>% 
      summarise(avg_tdn_delay = mean(tdn_delay, na.rm = T)) %>% na.omit -> obj$report$transition_durations.inc
  }
  return(obj$report[['transition_durations.inc']])
}

TRANSPORT.get.transition_durations.tdn.stn = function(obj){
  if(!is.null(obj$tables[['transition_durations.tdn.stn']])) obj$report[['transition_durations.tdn.stn']] <- obj$tables[['transition_durations.tdn.stn']]
  
  if(is.null(obj$report[['transition_durations.tdn.stn']])){
    obj$history %>% filter(selected) %>% 
      group_by(station, nextStation, TDN) %>% 
      summarise(meanTime = mean(duration, na.rm = T), sdTime = sd(duration, na.rm = T)) -> obj$report$transition_durations.tdn.station
  }
  return(obj$report[['transition_durations.tdn.stn']])
}

TRANSPORT.get.transition_durations.stn = function(obj){
  if(!is.null(obj$tables[['transition_durations.stn']])) obj$report[['transition_durations.stn']] <- obj$tables[['transition_durations.stn']]
  
  if(is.null(obj$report[['transition_durations.stn']])){
    obj$history %>% 
      group_by(station, nextStation) %>% 
      summarise(meanTime = mean(duration, na.rm = T), sdTime = sd(duration, na.rm = T)) -> obj$report$transition_durations.station
  }
  return(obj$report[['transition_durations.stn']])
}

encode_service_alterations = function(service_alterations){
  encoded = NULL
  sddown  = service_alterations$short_departures$dir == 'Down'
  sadown  = service_alterations$short_arrivals$dir   == 'Down'
  for(id in rownames(service_alterations$short_departures)[sddown]){
    encoded %<>% rbind(data.frame(caseID = id, rank = sequence(service_alterations$short_departures[id, 'rank'] - 1)))
  }
  for(id in rownames(service_alterations$short_departures)[!sddown]){
    encoded %<>% rbind(data.frame(caseID = id, rank = service_alterations$short_departures[id, 'rank'] + sequence(31 - service_alterations$short_departures[id, 'rank'])))
  }
  
  for(id in rownames(service_alterations$short_arrivals)[!sadown]){
    encoded %<>% rbind(data.frame(caseID = id, rank = sequence(service_alterations$short_arrivals[id, 'rank'] - 1)))
  }
  for(id in rownames(service_alterations$short_arrivals)[sadown]){
    encoded %<>% rbind(data.frame(caseID = id, rank = service_alterations$short_arrivals[id, 'rank'] + sequence(31 - service_alterations$short_arrivals[id, 'rank'])))
  }
  # for(id in service_alterations$cancelled){
  #   encoded %<>% rbind(data.frame(caseID = id, rank = sequence(31)))
  # }
  encoded %<>% rbind(service_alterations$bypass %>% select(caseID, rank) %>% as.data.frame)
  return(encoded)
}

TRANSPORT.apply.service_alterations.v0 = function(obj, service_alterations){
  scheduled = obj$objects$timetable$history %>% filter(status != "START") %>% filter(!(caseID %in% service_alterations$cancelled))
  rmv_head = function(tbl, lst){
    id = tbl$caseID[1]
    ww = which(tbl$station == lst[id, 'first_departure']) + 1
    tbl[ww:nn, ]
  }
  rmv_tail = function(tbl, lst){
    id = tbl$caseID[1]
    ww = which(tbl$station == lst[id, 'last_arrival'])
    tbl[1:ww, ]
  }
  
  scheduled %>% filter(caseID %in% rownames(service_alterations$short_departures)) %>% group_by(caseID) %>% 
    do({rmv_head(., service_alterations$short_departures)}) -> sd
}

TRANSPORT.apply.service_alterations.v1 = function(obj, service_alterations){
  scheduled = obj$objects$timetable$history %>% filter(status != "START") %>% filter(!(caseID %in% service_alterations$cancelled))
  saencoded = encode_service_alterations(service_alterations) %>% left_join(obj$tables$profile.station %>% select(station, rank), by = 'rank')
  scheduled %<>% anti_join(saencoded, by = c('caseID', 'station'))
  scheduled %<>% filter(!(scheduled$caseID %in% rownames(service_alterations$short_departures)) | duplicated(scheduled$caseID))
  # which(scheduled$caseID %in% rownames(service_alterations$short_departures) & !duplicated(scheduled$caseID))
  
  obj$objects$scheduled <- new('TRANSYS')
  obj$objects$scheduled$feed.eventlog(scheduled, extra_col = c('station', 'platform'), add_start = T)
  obj$objects$scheduled$history %>% dim
  
  obj$objects$scheduled$history %>% filter(status != 'START', nextStatus != 'END') %>% 
    left_join(obj$get.station_map() %>% rename(nextStatus = status, nextStation = station), by = 'nextStatus') %>% 
    anti_join(obj$history %>% filter(status != 'START', nextStatus != 'END'), by = c('caseID', 'station', 'nextStation')) -> errors
  
  obj$history %>% filter(status != 'START', nextStatus != 'END') %>% inner_join(errors %>% select(caseID, station), by = c('caseID', 'station'))
}

# TRANSPORT.get.transition_probabilities.tdn
TRANSPORT.get.transition_probabilities.tdn.alt = function(obj){
  if(is.null(obj$report[['transition_probabilities.tdn.alt']])){
    altered_cases = obj$tables$profile.case %>% filter(!is.na(alteration)) %>% pull(caseID)
    obj$objects$timetable$history %>% filter(caseID %in% altered_cases) %>% select(status, TDN, nextStatus) %>% 
      rbind(obj$history %>% filter(caseID %in% altered_cases) %>% select(status, TDN, nextStatus)) %>% 
      group_by(status, TDN, nextStatus) %>% 
      summarise(totalFreq = length(TDN)) %>% na.omit %>% 
      select(status, TDN, nextStatus, totalFreq) %>% arrange(status, TDN, nextStatus) %>% 
      group_by(status, TDN) %>% mutate(cum_freq = cumsum(totalFreq)) %>% 
      mutate(cum_prob = cum_freq/sum(totalFreq)) %>% ungroup() %>% 
      select(status, TDN, nextStatus, cum_prob) -> obj$report[['transition_probabilities.tdn.alt']]
  }
  return(obj$report[['transition_probabilities.tdn.alt']])
}

simulate.TRANSYS_old <- function(obj, start_dt, target_dt, transition_classifier = markovchain_transition_classifier, transition_time_estimator = markovchain_transition_time_estimator){
  start_dt     <- as.time(start_dt)
  target_dt    <- as.time(target_dt)
  final_events <- tibble()
  
  current_backlog = obj$history %>% filter(selected) %>% filter(startTime < start_dt) %>% as.tbl() %>%  
    group_by(caseID) %>% filter(startTime == max(startTime)) %>% ungroup %>% 
    filter(nextStatus != 'END') %>% 
    select(caseID, status, startTime) %>% arrange(caseID, startTime)
  
  new_starts = obj$history %>% filter(selected) %>% filter(startTime > start_dt, startTime < target_dt, status == 'START') %>% select(caseID, status, startTime) %>% as.tbl()
  
  historical_data <- obj$history %>% filter(selected) %>% filter(startTime < start_dt) %>% as.tbl() %>%  select(caseID, status, startTime) %>% unique
  if(is.empty(historical_data)) return(final_events)
  
  histobj <- TRANSYS()
  suppressWarnings({histobj$feed.eventlog(historical_data, caseStartTags = 'START', add_start = F)})
  
  # start simulation: 
  tracking <- rbind(current_backlog, new_starts)
  
  while(nrow(tracking) > 0) {
    tracking <- gen_next_events(tracking, histobj, transition_classifier) %>% 
      gen_transition_times(obj = histobj, start_dt = start_dt, transition_time_estimator = transition_time_estimator)
    # extract completed events and those that wont be completed before target_dt
    final_events <- rbind(final_events, tracking)
    # remove those rows from tracking and update
    tracking %<>% 
      filter(!(nextStatus == "End" | nxtTrTime > target_dt)) %>%
      transmute(caseID, status = nextStatus, startTime = nxtTrTime)
  }
  
  final_events %>% arrange(caseID, startTime)
}

TRANSPORT.run_simulation = function(obj, until = NULL, headway = T, headway_seconds = 120, ...){
  if(is.null(until)) until = obj$ctime + 24*3600 else until = as.time(until)
  assert(until > obj$ctime, "Cannot simulate to past! Argument 'until' must be greater than current time")
  lvls   = obj$tables$profile.station %>% arrange(rank) %>% pull(station)
  
  obj$tables$future <- obj$history %>% filter(startTime > obj$ctime) # temporary
  future_elog    <- obj$run.simulation(start_dt = obj$ctime, target_dt = until, event_generator = metrotrain_next_event_generator, time_generator = metrotrain_transition_time_generator, ...)

  if(headway){
    future_elog %<>% rename(endTime = nxtTrTime) %>% correct_events(obj = obj, headway_seconds = headway_seconds)
  }
  
  obj$objects$sim <- new('TRANSYS')
  obj$objects$sim$feed.eventlog(future_elog, add_start = T, remove_sst = T)
  obj$objects$sim$history <- obj$objects$sim$history %>% left_join(obj$tables$profile.case %>% select(caseID, TDN), by = 'caseID')
  # todo: add extra columns and feed tdn and date before
  obj$objects$sim_end <- until
}

TRANSPORT.get.performance = function(obj){
  TRANSPORT.get.service_alterations(obj) -> salt
  complete_services = obj$get.cases(full = T) %-% c(salt$cancelled, rownames(salt$short_departures), rownames(salt$short_arrivals), salt$bypass$caseID)
  timetable = obj$objects$timetable$history %>% filter(caseID %in% complete_services, nextStatus == 'END') %>% 
    select(caseID, station, scheduled_time = startTime)
  obj$history %>% filter(selected) %>% filter(caseID %in% complete_services, nextStatus == 'END') %>% select(caseID, station, actual_time = startTime) %>% 
    left_join(timetable, by = c('caseID', 'station')) %>% 
    mutate(delay = difftime(actual_time, scheduled_time, units = 'min')) -> compare
  reliability = 1 - mean(compare$delay > 5)
  nrs = length(obj$get.cases(full = T))
  delivery = (nrs - 0.25*length(rownames(salt$short_departures)) - 0.25*length(rownames(salt$short_arrivals)) - 0.125*length(rownames(salt$bypass)) - length(salt$cancelled))/nrs
  list(reliability = reliability, delivery = delivery)
}

get_metrics = function(events, timetable){
  # events    = obj$objects$sim$history %>% left_join(obj$get.station_map(), by = 'status')
  if(!'startDate' %in% colnames(events)) events %<>% mutate(startDate = as_date(startTime))
  if(!'startDate' %in% colnames(timetable)) timetable %<>% mutate(startDate = as_date(startTime))
  # timetable = obj$objects$timetable$history
  extract_service_alterations(events, timetable) -> salt
  
  complete_services = events$caseID %>% unique %-% c(salt$cancelled, salt$short_departures$caseID, salt$short_arrivals$caseID, salt$bypass$caseID)
  timetable = timetable %>% filter(caseID %in% complete_services, nextStatus == 'END') %>% 
    select(caseID, station, scheduled_time = startTime)
  events %>% filter(caseID %in% complete_services, nextStatus == 'END') %>% select(caseID, station, actual_time = startTime) %>% 
    left_join(timetable, by = c('caseID', 'station')) %>% 
    mutate(delay = difftime(actual_time, scheduled_time, units = 'min')) -> compare
  warnif(sum(is.na(compare$delay)) > 0, 'Delay could not be computed for all journeys!')
  reliability = 1 - mean(compare$delay > 5, na.rm = T)
  nrs = events$caseID %>% unique %>% length
  delivery = (nrs - 0.25*length(unique(salt$short_departures$caseID)) - 0.25*length(unique(salt$short_arrivals$caseID)) - 0.125*length(unique(salt$bypass$caseID)) - length(salt$cancelled))/nrs
  list(reliability = reliability, delivery = delivery)
}

TRANSPORT.get.sim_metrics = function(obj){
  events    = obj$objects$sim$history %>% left_join(obj$get.station_map(), by = 'status')
  timetable = obj$objects$timetable$history
  get_metrics(events, timetable)
}

TRANSPORT.get.service_alteration_probabilities.tdn = function(obj){
  if(is.null(obj$report[['service_alteration_probabilities.tdn']])){
    if(is.null(obj$tables$jp_full)){
      jp = obj$tables$profile.case
    } else {
      jp = obj$tables$jp_full
    }
    if(is.null(obj$tables$inc_full)){
      inc = obj$tables$profile.incident
    } else {
      inc = obj$tables$inc_full
    }
    
    jp %>% group_by(TDN, primary_flag, secondary_flag, sd_flag, sa_flag, bl_flag, c_flag) %>% summarise(cnt = length(caseID)) %>% 
      arrange(TDN, primary_flag, secondary_flag, sd_flag, sa_flag, bl_flag, c_flag) %>% 
      group_by(TDN, primary_flag, secondary_flag) %>% mutate(cum_freq = cumsum(cnt)) %>% 
      mutate(cum_prob = cum_freq/sum(cnt)) %>% ungroup() -> obj$report[['service_alteration_probabilities.tdn']]
    
    obj$report[['short_station_probabilities']] <- jp %>% 
      group_by(sd_flag, sa_flag, altst) %>% summarise(cnt = length(caseID)) %>% na.omit
  }
  return(obj$report[['service_alteration_probabilities.tdn']])
}

TRANSPORT.get.service_alteration_probabilities.dir = function(obj){
  if(is.null(obj$report[['service_alteration_probabilities.dir']])){
    if(is.null(obj$tables$jp_full)){
      jp = obj$tables$profile.case
    } else {
      jp = obj$tables$jp_full
    }
    if(is.null(obj$tables$inc_full)){
      inc = obj$tables$profile.incident
    } else {
      inc = obj$tables$inc_full
    }
    
    jp %>% group_by(dir, primary_flag, secondary_flag, sd_flag, sa_flag, bl_flag, c_flag) %>% summarise(cnt = length(caseID)) %>% 
      arrange(dir, primary_flag, secondary_flag, sd_flag, sa_flag, bl_flag, c_flag) %>% 
      group_by(dir, primary_flag, secondary_flag) %>% mutate(cum_freq = cumsum(cnt)) %>% 
      mutate(cum_prob = cum_freq/sum(cnt)) %>% ungroup() -> obj$report[['service_alteration_probabilities.dir']]
    
    obj$report[['short_station_probabilities']] <- jp %>% 
      group_by(sd_flag, sa_flag) %>% summarise(cnt = length(caseID)) %>% na.omit
  }
  return(obj$report[['service_alteration_probabilities.dir']])
}

# TRANSPORT.gen.random_service_alterations.dir = function(obj, caseIDs){
#   data.frame(caseID = caseIDs) %>% 
#     left_join(obj$tables$jp_full %>% select(caseID, dir, primary_flag, secondary_flag), by = 'caseID') %>% 
#     left_join(TRANSPORT.get.service_alteration_probabilities.dir(obj), by = c('dir', 'primary_flag', 'secondary_flag')) -> a1
#   
#   a1 %>% filter(!is.na(altst)) %>% 
#     left_join(obj$objects$timetable$history %>% select(caseID, station), by = 'caseID') %>% 
#     filter(altst == station) %>% select(-station) %>% distinct(caseID, sd_flag, sa_flag, c_flag, bl_flag, altst, .keep_all = T) %>% 
#     rbind(a1 %>% filter(is.na(altst))) %>% arrange(caseID, cnt) %>% group_by(caseID) %>% 
#     mutate(cum_freq = cumsum(cnt)) %>% mutate(cum_prob = cum_freq/sum(cnt)) %>% 
#     mutate(rand_var = runif(1)) %>% filter(rand_var < cum_prob) %>%
#     filter(cum_prob == min(cum_prob)) %>%
#     ungroup() %>% select(caseID, sd_flag, sa_flag, bl_flag, c_flag, altst)
# }


TRANSPORT.gen.random_service_alterations = function(obj, caseIDs, by_tdn = T){
  
  salt_prob = chif(by_tdn, TRANSPORT.get.service_alteration_probabilities.tdn(obj), TRANSPORT.get.service_alteration_probabilities.dir(obj))
  timetable = obj$objects$timetable$history %>% filter(caseID %in% caseIDs)
  
  data.frame(caseID = caseIDs) %>% 
    left_join(obj$tables$jp_full %>% select(caseID, TDN, dir, primary_flag, secondary_flag), by = 'caseID') %>% 
    left_join(salt_prob, by = c(chif(by_tdn,'TDN', 'dir'), 'primary_flag', 'secondary_flag')) %>% 
    group_by(caseID) %>% mutate(rand_var = runif(1)) %>% filter(rand_var < cum_prob) %>% filter(cum_prob == min(cum_prob)) %>% 
    select(-cnt, -cum_prob, -cum_freq, -rand_var) -> a1

  first_stations = timetable %>% group_by(caseID) %>% summarise(station = first(station))
  last_stations  = timetable %>% group_by(caseID) %>% summarise(station = last(station))
  
  a1 %>% filter(sa_flag | sd_flag) %>% 
    inner_join(obj$report[['short_station_probabilities']], by = c('sd_flag', 'sa_flag')) %>% 
    left_join(timetable %>% distinct(caseID, station), by = 'caseID') %>% 
    anti_join(first_stations, by = c('caseID', 'station')) %>% 
    anti_join(last_stations, by = c('caseID', 'station')) %>% 
    filter(altst == station) %>% arrange(caseID, cnt) %>% group_by(caseID) %>% 
    mutate(cum_freq = cumsum(cnt)) %>% mutate(cum_prob = cum_freq/sum(cnt)) %>% 
    mutate(rand_var = runif(1)) %>% filter(rand_var < cum_prob) %>%
    filter(cum_prob == min(cum_prob)) %>% 
    select(-cnt, -cum_prob, -cum_freq, -rand_var, -station) -> a2
    
  a1 %>% filter((!sa_flag) & (!sd_flag)) %>% mutate(altst = NA) %>% rbind(a2) %>% select(-TDN, -dir)
}

apply_service_alterations = function(service_alterations, obj, from = min(obj$objects$timetable$history$startTime) - 1, until = max(obj$objects$timetable$history$startTime) + 1){
  apply_sa = function(tbl){
    tbl %>% distinct(sd_flag, sa_flag, bl_flag, c_flag, altst) -> salt
    assert(nrow(salt) == 1)
    
    if (salt$c_flag){tbl = tbl[1,] %<>% filter(status == 'START') %>% mutate(nextStatus = 'END', endTime = startTime + 1)}
    
    nnn = nrow(tbl)  
    
    if (salt$sd_flag){
      w = which(tbl$station == salt$altst)
      if(length(w) > 0){
        w = min(w)
        tbl = tbl[w - 1 + sequence(nnn - w + 1),]
        tbl$status[1] = 'START'
        tbl$startTime[1] = tbl$endTime[1] - 1
        tbl$duration[1] = 1
      }
    }
    if (salt$sa_flag){
      w = which(tbl$station == salt$altst)
      if(length(w) > 0){
        w = min(w)
        if(w < 3){
          tbl = tbl[1,] %>% mutate(status = 'START', nextStatus = 'END', endTime = startTime + 1)
        } else {
          tbl = tbl[sequence(w),]
          nnn = nrow(tbl)  
          tbl$nextStatus[nnn] = 'END'
          tbl$endTime[nnn] = tbl$startTime[nnn] + 1
          tbl$duration[nnn] = 1
        }
      }
    }
    
    if(salt$bl_flag){
      loop_stations = c('PAR', 'MCE', 'FGS', 'SSS')
      tbl %<>% filter(!station %in% loop_stations)
    }
    return(tbl)
  }
  
  obj$objects$timetable$history %>% filter(startTime < until, startTime > from) %>% 
    left_join(service_alterations, by = 'caseID') -> a1 
  
  a1 %>% filter(sa_flag | sd_flag | c_flag | bl_flag) %>% 
    group_by(caseID) %>% 
    do({apply_sa(.)}) %>% 
    ungroup() %>% 
    rbind(a1 %>% filter(!sa_flag & !sd_flag & !c_flag & !bl_flag)) %>% 
    select(caseID, status, nextStatus, startTime, endTime, duration, station, nextStation, platform, TDN)
}

TRANSPORT.get.standard_transition_durations = function(obj){
  if(is.null(obj$report[['standard_transition_durations']])){
    obj$report[['standard_transition_durations']] <- obj$objects$timetable$history %>% group_by(station, nextStation) %>% na.omit %>% summarise(meanTime = mean(duration))
  }
  return(obj$report[['standard_transition_durations']])
}

TRANSPORT.get.transition_delays = function(obj){
  if(is.null(obj$report[['transition_delays']])){
    TRANSPORT.get.transitions(obj) %>% 
      left_join(obj$tables$jp_full %>% select(caseID, primary_flag, secondary_flag), by = 'caseID') %>% 
      mutate(incel = inc_elapsed > 0) %>% 
      left_join(TRANSPORT.get.standard_transition_durations(obj), by = c('station', 'nextStation')) %>% 
      mutate(delay = duration - meanTime) %>% 
      mutate(stmatch     = (st_from == station) | (st_to == station)) %>% 
      mutate(nxtstmatch  = (st_from == nextStation) | (st_to == nextStation)) %>% 
      group_by(transit, incel, stmatch, nxtstmatch, primary_flag, secondary_flag) %>% 
      summarise(avg_delay = mean(delay, na.rm = T), sd_delay = sd(delay, na.rm = T), cnt = length(delay)) %>% na.omit %>% 
      mutate(tstats = sqrt(cnt)*avg_delay/sd_delay) %>% 
      mutate(pvalue = round(1 - pt(tstats, df = cnt - 1),3)) %>% 
      filter(pvalue < 0.05) -> obj$report[['transition_delays']]
  }
  return(obj$report[['transition_delays']])
}

TRANSPORT.get.transition_delays.station = function(obj){
  if(is.null(obj$report[['transition_delays.station']])){
    TRANSPORT.get.transitions(obj) %>% 
      left_join(obj$tables$jp_full %>% select(caseID, primary_flag, secondary_flag), by = 'caseID') %>% 
      mutate(incel = inc_elapsed > 0) %>% 
      left_join(TRANSPORT.get.standard_transition_durations(obj), by = c('station', 'nextStation')) %>% 
      mutate(delay = duration - meanTime) %>% 
      mutate(stmatch     = (st_from == station) | (st_to == station)) %>% 
      mutate(nxtstmatch  = (st_from == nextStation) | (st_to == nextStation)) %>% 
      group_by(transit, incel, stmatch, nxtstmatch, primary_flag, secondary_flag, station, nextStation) %>% 
      summarise(avg_delay = mean(delay, na.rm = T), sd_delay = sd(delay, na.rm = T), cnt = length(delay)) %>% na.omit %>% 
      mutate(tstats = sqrt(cnt)*avg_delay/sd_delay) %>% 
      mutate(pvalue = round(1 - pt(tstats, df = cnt - 1),3)) %>% 
      filter(pvalue < 0.05) -> obj$report[['transition_delays.station']]
  }
  return(obj$report[['transition_delays.station']])
}

TRANSPORT.get.transition_delays.dow = function(obj){
  if(is.null(obj$report[['transition_delays.station']])){
    TRANSPORT.get.transitions(obj) %>% 
      mutate(dow = weekdays(startTime %>% as_date %>% as.Date)) %>% 
      left_join(obj$tables$jp_full %>% select(caseID, primary_flag, secondary_flag), by = 'caseID') %>% 
      mutate(incel = inc_elapsed > 0) %>% 
      left_join(TRANSPORT.get.standard_transition_durations(obj), by = c('station', 'nextStation')) %>% 
      mutate(delay = duration - meanTime) %>% 
      mutate(stmatch     = (st_from == station) | (st_to == station)) %>% 
      mutate(nxtstmatch  = (st_from == nextStation) | (st_to == nextStation)) %>% 
      group_by(transit, incel, stmatch, nxtstmatch, primary_flag, secondary_flag, dow) %>% 
      summarise(avg_delay = mean(delay, na.rm = T), sd_delay = sd(delay, na.rm = T), cnt = length(delay)) %>% na.omit %>% 
      mutate(tstats = sqrt(cnt)*avg_delay/sd_delay) %>% 
      mutate(pvalue = round(1 - pt(tstats, df = cnt - 1),3)) %>% 
      filter(pvalue < 0.05) -> obj$report[['transition_delays.dow']]
  }
  return(obj$report[['transition_delays.dow']])
}

TRANSPORT.feed_simulation = function(obj, elog){
  obj$objects$sim <- new('TRANSYS')
  obj$objects$sim$feed.eventlog(elog, add_start = F, remove_sst = T)
  obj$objects$sim$history <- obj$objects$sim$history %>% left_join(obj$tables$profile.case %>% select(caseID, TDN), by = 'caseID')
  # todo: add extra columns and feed tdn and date before
  obj$objects$sim_end <- max(elog$startTime)
}


# part 2: vistools:

# add_status_factor = function(eventlog, status_order = NULL, status_col = 'status', new_col = 'status_factor'){
#   scol = eventlog %>% pull(status_col)
#   if(is.null(status_order)){status_order = scol %>% unique} 
#   map = sequence(length(status_order))
#   names(map) <- status_order
#   eventlog[, new_col] <- map[scol] %>% as.factor %>% {levels(.) <- status_order;.}
#   eventlog
# }

TRANSYS.plot.case.journey = function(obj, case_id){
  
  obj$history %>% filter(caseID == case_id) %>% arrange(startTime) %>% add_status_factor %>% 
    viserPlot(x = 'startTime', y = 'status_factor', plotter = 'plotly', type = 'combo', shape = 'line.point')
}

TRANSYS.plot.case.journey = function(obj){

  tbl = obj$history %>% filter(selected) %>% to.status.caseID.startTime
  
  tbl %>% rownames2Column('status') %>% 
    mutate(status = factor(status, levels = obj$get.longest_path())) %>% arrange(status) %>% 
    viserPlot(x = 'status', y = colnames(tbl) %>% as.list, plotter = 'plotly', type = 'combo', shape = 'line.point')  
}

TRANSYS.plot.case.timeline = function(obj){
  tbl = obj$history %>% filter(selected) %>% select(start = startTime, content = status, end = endTime, group = caseID) %>% mutate(id = 1:nrow(.), type = 'range')
  tblgrp = data.frame(id = unique(tbl$group)) %>% mutate(content = id)
  timevis::timevis(data = tbl, groups = tblgrp)
}

cumulative.1 = function(tbl, columns = colnames(tbl), aggregator = c('sum', 'mean', 'min', 'max', 'last', 'prod')){
  aggregator = match.arg(aggregator)
  aggrfunmap = list(sum = cumsum, max = cummax, min = cummin, prod = cumprod, 
                    last = function(v) v %>% tibble %>% {colnames(.)<-'col';.} %>% tidyr::fill(col) %>% pull('col'))
  for(col in columns){
    tbl %<>% {.[, col] <- aggrfunmap[[aggregator]](pull(., col));.}
  }
  return(tbl)
}
cumulative.sum.2 = function(tbl, columns = colnames(tbl)){
  for(col in columns){
    tbl[, col] = cumsum(tbl[, col])
  }
  return(tbl)
}

#    For a given case, for each status we need to find out at every time, has the case so far met this status or not? caseAtTime.status.entry()
#         1- cast history startTime + caseID ~ status , aggr.func = count, value.var = any
#         2- group by caseID %>% do sort by startTime and replace each column by its cumulative sum -> this gives you matrix of status entries per case
#         3- left_join eventlog with this table by startTime and caseID 
TRANSYS.get.caseAtTime.status.entry = function(obj){
  if(is.null(obj$tables$caseAtTime.status.entry)){
    obj$history %>% filter(selected) %>% 
      reshape2::dcast(caseID + startTime ~ status, fun.aggregate = length, value.var = 'endTime') %>% 
      arrange(caseID, startTime) -> res
    
    if(nrow(res) < 2){return(res)}
    
    # t1 = Sys.time()
    res %>% select(-startTime, -caseID) %>% as.matrix -> M
    
    dp = duplicated(res$caseID)
    for(i in 2:nrow(M)){
      M[i,] <- M[i,] + dp[i]*M[i - 1,]      
    }
    res %>% select(caseID, startTime) %>% cbind(M) -> obj$tables$caseAtTime.status.entry
    
    # res %>% arrange(caseID, startTime) %>% group_by(caseID) %>% do({cumulative.3(., columns = ns)}) %>% as.data.frame -> res2
    # t2 = Sys.time()
  }
  return(obj$tables$caseAtTime.status.entry)
}


#    for each status we need to find out at the decision-time, if the case has met this status, when the case has exited that status?
#         1- cast history startTime + caseID ~ status , aggr.func = latest(sum or mean should also give the same result becasue there is most-likely only one value), value.var = endTime
#         2- group by caseID %>% do sort by startTime and replace each column by its cumulative latest or cumulative max -> this gives you matrix of status exit times per case (NA if never entered pr not exited)
TRANSYS.get.caseAtTime.status.latestExitTime = function(obj, as_numeric = F){
  if(is.null(obj$tables$caseAtTime.status.latestExitTime)){
    obj$history %>% filter(selected) %>% 
      reshape2::dcast(caseID + startTime ~ status, fun.aggregate = last, value.var = 'endTime') %>% 
      arrange(caseID, startTime) -> res

    if(nrow(res) < 2){return(res)}
    
    res %>% select(-startTime, -caseID) %>% as.matrix -> M
    
    dp = duplicated(res$caseID)
    for(i in 2:nrow(M)){
      M[i,] <- ifelse(is.na(M[i,]) & dp[i], M[i - 1,], M[i,])
    }
    
    res %>% select(caseID, startTime) %>% cbind(M) -> obj$tables$caseAtTime.status.latestExitTime  
  }
  
  if(!as_numeric){
    return(obj$tables$caseAtTime.status.latestExitTime %>% {for(col in colnames(.) %-% c('caseID', 'startTime')){.[,col] %<>% as.POSIXct(origin = '1970-01-01')};.})
  } else {
    return(obj$tables$caseAtTime.status.latestExitTime)
  }
}

TRANSPORT.get.transition_durations.transit = function(obj){
  if(is.null(obj[['report$transition_durations.transit']])){
    obj$history %>% 
      # filter(endTime < obj$ctime) %>% 
      add_transit_column %>% 
      group_by(transit) %>% summarise(meanTime = mean(duration, na.rm = T)) -> obj[['report$transition_durations.transit']]
  }
  return(obj[['report$transition_durations.transit']])
}

TRANSPORT.add.transition_features.scheduled = function(obj){
  
  a = TRANSPORT.get.transitions(obj) %>% 
    spark.unselect('startTime_sch', 'endTime_sch', 'duration_sch', 'time2Scheduled', 'startTime_delay_sch', 'endTime_delay_sch', 'deviation_sch') %>%  
    left_join(obj$objects$timetable$history %>% select(caseID, status, nextStatus, startTime_sch = startTime, endTime_sch = endTime), by = c('caseID', 'status', 'nextStatus')) %>% 
    mutate(duration_sch = difftime(endTime_sch, startTime_sch, units = 'secs') %>% as.numeric)
  
  wna = which(is.na(a$duration_sch))
  if(length(wna) > 0){
    scheduled_durations = obj$objects$timetable$history %>% group_by(status, nextStatus) %>% summarise(schdur = mean(duration, na.rm = T))
    b = a[wna, ] %>% left_join(scheduled_durations, by = c('status', 'nextStatus'))
    a[wna, 'startTime_sch'] <- b$startTime_sch
    a[wna, 'endTime_sch']   <- b$endTime_sch
    a[wna, ]  %<>% mutate(duration_sch = difftime(endTime_sch, startTime_sch, units = 'secs') %>% as.numeric)
  }
  obj$tables$profile.transition <- a %>% 
    mutate(time2Scheduled = difftime(endTime_sch, startTime, units = 'secs') %>% as.numeric, 
           startTime_delay_sch = difftime(startTime, startTime_sch, units = 'secs') %>% as.numeric,
           endTime_delay_sch   = difftime(endTime  , endTime_sch  , units = 'secs') %>% as.numeric,
           deviation_sch       = duration - duration_sch)
}

TRANSPORT.add.transition_features.meanTime = function(obj){
  
  if(!(c('station', 'nextStation') %<% colnames(TRANSPORT.get.transitions(obj)))){TRANSPORT.add.transition_features.station(obj)}
  
  mean_durations = TRANSPORT.get.transition_durations.tdn(obj)
  
  a = TRANSPORT.get.transitions(obj) %>% spark.unselect('meanTime') %>%  
    left_join(mean_durations, by = c('status', 'nextStatus', 'TDN')) %>% 
    mutate(duration_sch = difftime(endTime_sch, startTime_sch, units = 'secs') %>% as.numeric)
  
  wna = which(is.na(a$meanTime))
  if(length(wna) > 0){
    mean_durations = TRANSPORT.get.transition_durations.tdn.stn(obj)
    a[wna, 'meanTime'] = a[wna, ] %>% left_join(mean_durations, by = c('station', 'nextStation')) %>% pull(meanTime)
  }
  
  wna = which(is.na(a$meanTime))
  if(length(wna) > 0){
    mean_durations = obj$get.links()
    a[wna, 'meanTime'] = a[wna, ] %>% left_join(mean_durations, by = c('status', 'nextStatus')) %>% pull(meanTime)
  }
  
  wna = which(is.na(a$meanTime))
  if(length(wna) > 0){
    mean_durations     = TRANSPORT.get.transition_durations.transit(obj)
    a[wna, 'meanTime'] = a[wna, ] %>% left_join(mean_durations, by = 'transit') %>% pull(meanTime)
  }

  obj$tables$profile.transition <- a %>% mutate(deviation = duration - meanTime)
}

TRANSPORT.get.transitions = function(obj){
  if(is.null(obj$tables$profile.transition)){
      obj$tables$profile.transition <- TRANSYS.get.transitions(obj) %>%  
        add_transit_column
  }
  return(obj$tables$profile.transition)
}

TRANSYS.get.transitions = function(obj, features = NULL){
  base_columns = c('caseID', 'startTime', 'status', 'nextStatus', 'eventAge', 'duration', 'selected')
  features %<>% verify('character', default = character()) %>% setdiff(base_columns)
  if(is.null(obj$tables$profile.transition)){
    obj$tables$profile.transition <- obj$history %>% 
      spark.select(base_columns, features)
  }
  return(obj$tables$profile.transition)
}

TRANSPORT.get.date.incType.volume = function(obj){
  if(is.null(obj$tables$date.incType.volume)){
    if(!is.null(obj$tables$incident.profile)){
      obj$tables$profile.incident %>% 
        reshape2::dcast(incident_date ~ inctype, value.var = 'incid', fun.aggregate = length) %>% 
        arrange(incident_date) -> obj$tables$date.incType.volume
    } else (stop('No incident profile feeded!'))
  }
  return(obj$tables$date.incType.volume)
}

TRANSPORT.add.transition_features.station = function(obj){
  maxrank = max(obj$tables$profile.station$rank)
  
  obj$tables$profile.transition <- TRANSPORT.get.transitions(obj) %>% 
    spark.unselect('station', 'nextStation', 'rank', 'nextRank') %>% 
    left_join(obj$get.station_map(), by = 'status') %>% 
    left_join(obj$get.station_map() %>% rename(nextStatus = status, nextStation = station), by = 'nextStatus') %>%
    left_join(obj$tables$profile.station %>% select(station, rank, x1 = longitude, y1 = latitude), by = 'station') %>% 
    left_join(obj$tables$profile.station %>% select(nextStation = station, nextRank = rank, x2 = longitude, y2 = latitude), by = 'nextStation') %>% 
    mutate(dist = geosphere::distGeo(p1 = select(.,x1, y1), p2 = select(.,x2, y2))) %>% 
    {.$rank[.$status == 'START'] <- 0;.$nextRank[.$status == 'END'] <- maxrank + 1;.} %>% 
    select(-x1, -y1, -x2, -y2)
}

TRANSPORT.add.transition_features.incident = function(obj){
  # TRANSPORT.add.transition_features.station(obj)
  # if(colnames(tables$))
  obj$get.incidents() %>% 
    left_join(obj$tables$profile.station %>% rename(st_from = station, str_from = rank), by = 'st_from') %>% 
    left_join(obj$tables$profile.station %>% rename(st_to = station, str_to = rank), by = 'st_to') %>% 
    mutate(trans_date = as_date(time_from)) %>% 
    select(trans_date, inctype, str_from, str_to, inctime = time_from) -> inc
  
  tflong <- TRANSPORT.get.transitions(obj) %>% 
    spark.unselect(colnames(inc)) %>% 
    mutate(trans_date = as_date(startTime)) %>% full_join(inc, by = 'trans_date') %>% 
    filter(startTime > inctime) %>% 
    mutate(dist = ifelse(rank < nextRank, 
                        ifelse(str_from < str_to, str_to - rank, str_from - rank),
                        ifelse(str_from < str_to, rank - str_from, rank - str_to))) %>% filter(dist >= 0) %>% 
    select(caseID, startTime, status, nextStatus, inctype, dist, inctime)
 
  TRANSPORT.get.transitions(obj) %>% 
    select(caseID, startTime, status, nextStatus, rank, nextRank, duration) %>% 
    full_join(tflong, by = c('caseID', 'startTime', 'status', 'nextStatus')) -> tf
  
  # tf.free =  tf %>% filter(is.na(inctype))
  tf.inc  =  tf %>% filter(!is.na(inctype)) 
  
  tf.inc.count <- tf.inc %>% 
    reshape2::dcast(caseID + startTime + status + nextStatus ~ inctype, value.var = 'inctime', fun.aggregate = length) %>% 
    {names(.)[5:ncol(.)] %<>% paste('count', sep = '_');.}
  
  tf.inc.elapsed <- tf.inc %>% mutate(elapsed = as.numeric(startTime - inctime)) %>% 
    reshape2::dcast(caseID + startTime + status + nextStatus ~ inctype, value.var = 'elapsed', fun.aggregate = last) %>% 
    {names(.)[5:ncol(.)] %<>% paste('elapsed', sep = '_');.}
  
  tf.inc.dist <- tf.inc %>% 
    reshape2::dcast(caseID + startTime + status + nextStatus ~ inctype, value.var = 'dist', fun.aggregate = last) %>% 
    {names(.)[5:ncol(.)] %<>% paste('dist', sep = '_');.}

  TRANSPORT.get.transitions(obj) %>% 
    spark.unselect(obj$tables$profile.incident$inctype %>% unique %>% paste('elapsed', sep = '_'),
                   obj$tables$profile.incident$inctype %>% unique %>% paste('dist', sep = '_'),
                   obj$tables$profile.incident$inctype %>% unique %>% paste('count', sep = '_')) %>% 
    left_join(tf.inc.count,   by = c('caseID', 'startTime', 'status', 'nextStatus')) %>% 
    left_join(tf.inc.elapsed, by = c('caseID', 'startTime', 'status', 'nextStatus')) %>% 
    left_join(tf.inc.dist,    by = c('caseID', 'startTime', 'status', 'nextStatus')) -> obj$tables$profile.transition
}

TRANSYS.filter.event = function(obj, ...){
  obj$history$row = 1:nrow(obj$history)
  rows = obj$history %>% filter(...) %>% pull(row)
  obj$history$selected = obj$history$selected & (obj$history$row %in% rows)
  obj$clear()
}

TRANSYS.add.transition_features.case = function(obj, features){
  features %<>% verify('character', domain = colnames(obj$tables$profile.case)) %>% setdiff('caseID')
  
  obj$tables$profile.transition <- TRANSYS.get.transitions(obj) %>% 
    spark.unselect(features) %>% 
    left_join(obj$tables$profile.case %>% spark.select('caseID', features), by = 'caseID')
}

TRANSPORT.add.case_features.incident = function(obj){
  features = c('inctype', 'time_from', 'st_from', 'st_to')
  obj$tables$profile.case <- obj$tables$profile.case %>% 
    spark.unselect(features) %>% 
    left_join(obj$tables$profile.incident %>% select(caseID, inctype, time_from, st_from, st_to) %>% distinct(caseID, .keep_all = T), by = 'caseID')
}

TRANSPORT.build.model = function(obj){
  TRANSPORT.get.transitions(obj)
  TRANSPORT.add.transition_features.station(obj)
  TRANSPORT.add.transition_features.case(obj)
  TRANSPORT.add.transition_features.incident(obj)
  
  # features = c('status', 'nextStatus', 'dir','line','band', 'vdt','prvat', 'altcd', 'planned_alt', 'inctype', 'time_from', 'st_from', 'st_to', 'inc_elapsed')
  part <- TRANSPORT.get.transitions(obj) %>% 
    mutate(incexist = as.integer(!is.na(inctype))) %>% 
    add_transit_column %>% 
    mutate(transit = as.integer(transit)) %>% 
    mutate(inctype = ifelse(is.na(inctype), "NOINC", inctype)) %>% 
    partition(0.7)
  
  X_train <- part$part1 %>% select(-caseID, -startTime, -duration, -selected) %>% integer2numeric
  y_train <- part$part1 %>% pull(duration)
  X_test  <- part$part2 %>% select(-caseID, -startTime, -duration, -selected) %>% integer2numeric
  y_test  <- part$part2 %>% pull(duration)
  
  which(X_test$transit == 0 & X_test$incexist == 0) -> w00
  which(X_test$transit == 0 & X_test$incexist == 1) -> w01
  which(X_test$transit == 1 & X_test$incexist == 0) -> w10
  which(X_test$transit == 1 & X_test$incexist == 1) -> w11
  which(X_test$transit == 2 & X_test$incexist == 0) -> w20
  which(X_test$transit == 2 & X_test$incexist == 1) -> w21
  which(X_test$transit == 3 & X_test$incexist == 0) -> w30
  which(X_test$transit == 3 & X_test$incexist == 1) -> w31

  segmentation = c('transit', 'incexist')
  
  segmentation = c('status', 'nextStatus')
  
  cc    = CATCONCATER(name = 'path', concatenating_features = segmentation, keep_columns = F)
  dm    = DUMMIFIER(dummified_features = nominals(X_train) %-% segmentation, filter_columns = segmentation)
  model = SEGMENTER.MODEL(model_class = 'REG.LM', transformers = list(cc, dm), model_config = list(rfe.enabled = T), build_global_model = F, min_rows = 10, segmentation_features = 'path_out')

  # segmentation = 'transit'
  # X_train[, 'transit'] %<>% as.integer 
  
  # dm    = DUMMIFIER(dummified_features = c('station', 'nextStation'))
  # model = SEGMENTER.MODEL(model_class = 'REG.LM', transformers = dm, model_config = list(rfe.enabled = T), build_global_model = F, min_rows = 25, segmentation_features = segmentation)
  
  # model = REG.LM(transformers = DUMMIFIER(name = 'DM'), rfe.enabled = T)
  # debug(model$fit)
  model$fit(X_train, y_train)
  model$predict(X_test) %>% cbind(y_test) -> res
  sd(res[,1] - res[,2], na.rm = T)

  mean(abs(res[,1] - res[,2]), na.rm = T)
  median(abs(res[,1] - res[,2]), na.rm = T)
  
  mean(abs(res[w00,1] - res[w00,2]), na.rm = T)
  median(abs(res[w00,1] - res[w00,2]), na.rm = T)
  
  mean(abs(res[w01,1] - res[w01,2]), na.rm = T)
  median(abs(res[w01,1] - res[w01,2]), na.rm = T)

  mean(abs(res[w10,1] - res[w10,2]), na.rm = T)
  median(abs(res[w10,1] - res[w10,2]), na.rm = T)

  mean(abs(res[w11,1] - res[w11,2]), na.rm = T)
  median(abs(res[w11,1] - res[w11,2]), na.rm = T)
  
  mean(abs(res[w31,1] - res[w31,2]), na.rm = T)
  median(abs(res[w31,1] - res[w31,2]), na.rm = T)
  
  
  lookup <- cbind(X_train, y = y_train) %>% group_by(status, nextStatus, TDN, inctype) %>% summarise(meanTime = mean(y), medTime = median(y))
  X_test %>% cbind(y = y_test) %>% left_join(lookup, by = c('status', 'nextStatus', 'TDN', 'inctype')) %>% select(meanTime, y) -> res
}

# add.incident.features(obj) %>% View
TRANSPORT.add.transition_features.case = function(obj){
  features = c('dir','line','band', 'vdt', 'altcd', 'planned_alt', 'inctype', 'time_from', 'st_from', 'st_to')
  TRANSPORT.add.case_features.incident(obj)
  TRANSPORT.get.transitions(obj)
  TRANSYS.add.transition_features.case(obj, features = features)

  obj$tables$profile.case %>% 
    mutate(prvat = as.numeric(difftime(praat, prsat, units = 'mins'))) %>% 
    mutate(planned_alt = replace(planned_alt, planned_alt == 'nan', NA), altcd = replace(altcd, altcd == 'nan', NA)) %>% 
    mutate(planned_alt = gsub(planned_alt, pattern = ',', replacement = '__'), altcd = gsub(altcd, pattern = ',', replacement = '__')) %>% 
    mutate() -> obj$tables$profile.case
  
  TRANSYS.add.transition_features.case(obj, features = 'prvat')

  obj$tables$profile.transition <- TRANSPORT.get.transitions(obj) %>% 
    mutate(inc_elapsed = difftime(startTime, time_from, units = 'mins') %>% as.numeric) %>% select(-time_from)
  
  
  
  # # maxrank = max(obj$tables$profile.transition$rank, obj$tables$profile.transition$nextRank, na.rm = T)
  # obj$tables$profile.transition %>% 
  #   # {.$rank[.$status == 'START']<-0; .$nextRank[.$nextStatus == 'END']<-maxrank+1;.} %>% 
  #   select(status, nextStatus) %>% cbind(tf) %>% 
  #   string2factor %>% 
  #   select(-caseID) %>% partition -> part 
  # 
  # X_train = part$part1 %>% select(-nextStatus) %>% factor2integer
  # y_train = part$part1 %>% pull(nextStatus)
  # 
  # X_test = part$part2 %>% select(-nextStatus) %>% factor2integer
  # y_test = part$part2 %>% pull(nextStatus)
  # 
  # sm = SEGMENTER.MODEL(model_class = "CLS.MLR", model_config = list(model_type = "classif.naiveBayes"), segmentation_featres = 'status')
  # debug(sm$fit)
  # sm$fit(X_train, y_train)
  # 
  # # Incomplete: need to work on it
  # 
  # 
  #   
  # # nb = CLS.E1071.NB()
  # # nb$fit(X_train, y_train)  
  # 
  # # naiveBayes(nextStatus ~ ., data = part$part1) -> nb
  # # predict(nb, X_test) -> yp
  # # cbind(y_test, yp) %>% View
  # # sum(yp == y_test)/length(yp == y_test)
  # 
  # # C5.0(x = X_train, y = y_train) -> mdl
  # 
  # mdl = rpart(Y ~ ., data = cbind(X_train, Y = y_train), method = 'class', control = rpart.control(minsplit = 2000, minbucket = 1000, maxdepth = 20, cp = -1))
  # predict(mdl, X_test, type = "class") -> yp
  # sum(yp == y_test)/length(yp == y_test)
  # 
  # 
  # mdl$fit(X_train, y_train)
  # 
  # 
  # 
  # mdl = CLS.SCIKIT.XGB()
  # mdl$fit(X_train[1:1000,], y_train[1:1000] %>% as.integer)
  # mdl$predict(X_train[1:100,], F) -> yp
  # 
  # 
  # if(identical(tf$caseID, get.profile.transition(obj) %>% pull(caseID))){
  #   get.profile.transition(obj) %>% cbind(tf %>% select(-caseID)) -> obj$tables$profile.transition
  # } else stop("This must not happen!")
  # 
  # 
  
}

TRANSYS.add.network.features = function(obj){
  tsv = obj$tables$time.status.volume %>% as.data.frame %>% {colnames(.) %<>% paste('volume', sep = '_');.} %>% rownames2Column('startTime') %>% mutate(startTime = as.POSIXct(startTime))
  get.transitions(obj) %>% select(startTime) %>% left_join(tsv, by = 'startTime') %>% dim
}


# Generates ML data for training classifier model to prdict probabilities for next transition given caseID, current time, and current status
TRANSYS.add.transition.features = function(obj){
  obj$tables$profile.transition <- obj$history %>% select(caseID, startTime, status, nextStatus)
  # are there any case static features?
  if(!is.null(tables$profile.case)){
    obj$tables$transition.features <- obj$tables$transition.features %>% left_join(obj$tables$case.features, by = 'caseID')
  }
  
  # should I add time seasonality features?
  if(!is.null(tables$time.features)){
    tables$mldata.tc <<- tables$mldata.tc %>% left_join(time.features, by = startTime)
  }
  # should I add status.time volume features?
  if(!is.null(tables$time.status.volume)){
    tables$mldata.tc <<- tables$mldata.tc %>% left_join(time.status.volume, by = startTime)
  }
  # should I add status features?
  if(!is.null(tables$status.features)){
    tables$mldata.tc <<- tables$mldata.tc %>% left_join(status.features, by = status)
  }
  # should I add status.time features?
  if(!is.null(tables$status.time.features)){
    tables$mldata.tc <<- tables$mldata.tc %>% left_join(status.time.features, by = c(status, startTime))
  }
  return(tables$mldata.tc)
}

#obj$tables$profile.transition %>% select(-caseID, -startTime, -status, -nextStatus, -duration) %>% partition -> part

# returns service alterations in list format
extract_service_alterations = function(events, timetable){
  if(is.null(events$startDate)){events$startDate = as_date(events$startTime)}
  services  = timetable %>% filter(startDate %in% events$startDate) %>% pull(caseID) %>% unique
  timetable = timetable %>% filter(caseID %in% services)
  
  cancelflg = (events$status == 'START') & (events$nextStatus == 'END')
  cancelled = events$caseID[cancelflg] %>% unique

  timetable %>% 
    filter(!(caseID %in% cancelled), status == 'START') %>% 
    select(caseID, scheduled_first_departure = nextStation) %>% 
    left_join(events[!cancelflg,] %>% filter(status == 'START') %>% select(caseID, nextStation), by = 'caseID') %>% 
    filter(nextStation != scheduled_first_departure) %>% rename(actual_first_departure = nextStation) -> short_departures
  
  timetable %>% 
    filter(!(caseID %in% cancelled), nextStatus == 'END') %>% 
    select(caseID, scheduled_last_arrival = station) %>% 
    left_join(events[!cancelflg,] %>% filter(nextStatus == 'END') %>% select(caseID, station), by = 'caseID') %>% 
    filter(station != scheduled_last_arrival) %>% rename(actual_last_arrival = station) -> short_arrivals

  timetable %>% 
    filter(!(caseID %in% c(cancelled, short_departures$caseID, short_arrivals$caseID)), status != 'START', nextStatus!= 'END') %>% 
    anti_join(events[!cancelflg,], by = c('caseID', 'station')) %>% arrange(caseID, startTime) %>% 
    group_by(caseID) %>% summarise(skipped_stations = station %>% unique %>% paste(collapse = ',')) %>% ungroup -> bypass

  list(cancelled = cancelled, short_departures = short_departures, short_arrivals = short_arrivals, bypass = bypass)
}

# returns service alterations in table format
extract_service_alterations.table = function(events, timetable){
  salt_lst = extract_service_alterations(events, timetable)
  salt_tbl = data.frame(caseID = unique(events$caseID), c_flag = F, sd_flag = F, sa_flag = F, bl_flag = F, altst = NA) %>% column2Rownames('caseID')
  salt_tbl[salt_lst$cancelled %>% as.character, 'c_flag'] <- T
  salt_tbl[salt_lst$short_departures$caseID, 'sd_flag']   <- T
  salt_tbl[salt_lst$short_departures$caseID, 'altst']     <- salt_lst$short_departures$actual_first_departure %>% as.character
  salt_tbl[salt_lst$short_arrivals$caseID, 'sa_flag']     <- T
  salt_tbl[salt_lst$short_arrivals$caseID, 'altst']       <- salt_lst$short_arrivals$actual_last_arrival %>% as.character
  bl_cases = salt_lst$bypass %>% filter(skipped_stations %in% c('PAR,MCE,FGS,SSS', 'SSS,FGS,MCE,PAR')) %>% pull(caseID)
  salt_tbl[bl_cases, 'bl_flag'] <- T
  return(salt_tbl)
}

# returns service alterations in list format
TRANSPORT.get.service_alterations = function(obj){

  res = extract_service_alterations(obj$history %>% filter(selected), obj$objects$timetable$history)
  
  obj$tables$profile.case$alteration = NA
  
  w = which(obj$tables$profile.case$caseID %in% res$cancelled)
  assert(length(w) == length(res$cancelled))
  obj$tables$profile.case$alteration[w] <- 'C'
  
  w = which(obj$tables$profile.case$caseID %in% rownames(res$short_departures))
  assert(length(w) == nrow(res$short_departures))
  obj$tables$profile.case$alteration[w] <- 'SD' %>% paste(res$short_departures$station, sep = '@')
  
  w = which(obj$tables$profile.case$caseID %in% rownames(res$short_arrivals))
  assert(length(w) == nrow(res$short_arrivals))
  obj$tables$profile.case$alteration[w] <- 'SA' %>% paste(res$short_arrivals$station, sep = '@')

  w = which(obj$tables$profile.case$caseID %in% res$bypass$caseID)
  obj$tables$profile.case$alteration[w] <- 'BP'
    
  list(cancelled = res$cancelled, short_departures = res$short_departures, short_arrivals = res$short_arrivals, bypass = res$bypass)
}

predict_durations_V0 = function(input, obj){
  inp = input %>% 
    left_join(obj$tables$profile.case %>%
                select(caseID, inctype, time_from, st_from, st_to, primary_flag, secondary_flag), by = 'caseID') %>% 
    mutate(incel       = time_from > startTime) %>% 
    mutate(stmatch     = (st_from == station) | (st_to == station), nxtstmatch  = (st_from == nextStation) | (st_to == nextStation)) %>% 
    add_transit_column %>% 
    left_join(TRANSPORT.get.transition_delays(obj), by = c('incel', 'transit', 'stmatch', 'nxtstmatch', 'primary_flag', 'secondary_flag'))
    
  wna = which(is.na(inp$avg_delay))
  inp$avg_delay[wna] <- 0
  
  inp$duration = inp$duration + inp$avg_delay
  
  inp %>% 
    mutate(duration_org = duration, startTime_org = startTime, endTime_org = endTime) %>% 
    mutate(duration = duration + avg_delay) %>% select(-avg_delay) %>% 
    arrange(caseID, startTime) %>% group_by(caseID) %>%  
    mutate(firstTime = first(startTime), cum_dur = cumsum(duration)) %>% 
    mutate(endTime = firstTime + cum_dur) %>% 
    mutate(startTime = endTime - duration)
}

predict_durations_V2 = function(input, obj){

  inp = input %>% 
    left_join(TRANSPORT.get.transition_durations.tdn(obj), by = c("status", "nextStatus", "TDN"))
  
  tbd = which((inp$status == 'START') | (inp$nextStatus == 'END')) 
  inp$meanTime[tbd] <- 0.1
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = TRANSPORT.get.transition_durations.tdn.stn(obj)
    
    inp$meanTime[wna] <- inp[wna,] %>% select(station, nextStation, TDN) %>%
      # left_join(obj$get.station_map(), by = 'status') %>% 
      # left_join(obj$get.station_map() %>% rename(nextStatus = status, nextStation =station), by = 'nextStatus') %>% 
      left_join(ad_base, by = c('station', 'nextStation', 'TDN')) %>% pull(meanTime)
  }
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = obj$get.links()
    
    inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus) %>% 
      left_join(ad_base, by = c('status', 'nextStatus')) %>% pull(meanTime)
  }
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base <- obj$objects$timetable$get.links() %>% 
      select(status, nextStatus, meanTime)
    
    inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus) %>% 
      left_join(ad_base, by = c('status', 'nextStatus')) %>% pull(meanTime)
  }
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = TRANSPORT.get.transition_durations.stn(obj)
    
    inp$meanTime[wna] <- inp[wna,] %>% select(status, nextStatus) %>%
      left_join(obj$get.station_map(), by = 'status') %>% 
      left_join(obj$get.station_map() %>% rename(nextStatus = status, nextStation =station), by = 'nextStatus') %>% 
      left_join(ad_base, by = c('station', 'nextStation')) %>% pull(meanTime)
  }
  
  wna = is.na(inp$meanTime) %>% which
  if(length(wna) > 0){
    ad_base = TRANSPORT.get.transition_durations.transit(obj)
    inp$meanTime[wna] <- inp[wna,] %>% add_transit_column %>% 
      select(transit) %>%
      left_join(ad_base, by = 'transit') %>% pull(meanTime)
  }
  
  assert(sum(is.na(inp$meanTime)) == 0)
  
  return(inp %>% rename(duration_prd = meanTime))
}

predict_durations_divine = function(input, obj){

  inp = input %>% 
    left_join(obj$history %>% select(caseID, station, nextStation, duration_prd = duration), by = c("caseID", "station", "nextStation"))
  
  tbd = which((inp$status == 'START') | (inp$nextStatus == 'END')) 
  inp$duration_prd[tbd] <- 0.1
  
  wna = is.na(inp$duration_prd) %>% which
  if(length(wna) > 0){
    inp[wna, 'duration_prd'] <- inp[wna, ] %>% select(-duration_prd) %>% predict_durations_V2(obj) %>% pull(duration_prd)
  }
  
  return(inp)      
}

predict_startTime_divine = function(input, obj){
  act_st = obj$history %>% select(caseID, nextStation, startTime) %>% distinct(caseID, nextStation, .keep_all = T)
  sch_st = obj$objects$timetable$history %>% anti_join(act_st, by = c('caseID', 'nextStation')) %>% 
    select(caseID, nextStation, startTime) %>% distinct(caseID, nextStation, .keep_all = T)
  wstart = which(input$status == 'START')
  input[wstart, 'startTime'] <- input[wstart, ] %>% select(caseID, nextStation) %>% 
    left_join(rbind(act_st, sch_st), by = c('caseID', 'nextStation')) %>% pull(startTime)
  
  input %>% group_by(caseID) %>%  
    mutate(firstTime = first(startTime), cum_dur = cumsum(duration)) %>% 
    mutate(endTime = firstTime + cum_dur) %>% 
    mutate(startTime = endTime - duration) %>% 
    select(-firstTime, -cum_dur)
  
  # dp = duplicated(input$caseID)
  # assert(identical(wstart, which(!dp)))
  # input$startTime[which(dp)] <- input[dp,] %>% select(-startTime) %>% 
  #   left_join(input[wstart,] %>% select(caseID, startTime), by = 'caseID') %>% pull(startTime)
}

predict_startTime_scheduled = function(input, obj){
  sch_st = obj$objects$timetable$history %>% filter(status == 'START') %>% select(caseID, nextStation, startTime)
  act_st = obj$history %>% filter(status == 'START') %>% anti_join(sch_st, by = c('caseID', 'nextStation')) %>% 
    select(caseID, nextStation, startTime)
  wstart = which(input$status == 'START')
  input[wstart, 'startTime'] <- input[wstart, ] %>% select(caseID, nextStation) %>% left_join(rbind(act_st, sch_st), by = c('caseID', 'nextStation')) %>% pull(startTime)
  input %>% group_by(caseID) %>%  
    mutate(firstTime = first(startTime), cum_dur = cumsum(duration)) %>% 
    mutate(endTime = firstTime + cum_dur) %>% 
    mutate(startTime = endTime - duration) %>% 
    select(-firstTime, -cum_dur)
}

correct_durations_V2 = function(input, obj){
  ## Correction: Add delays to transition durations for journeys impacted by incidents as primary journey:
  jwincs    = obj$tables$profile.incident %>% pull(caseID) %>% unique
  tocorr    = which((input$caseID %in% jwincs) & (input$status != 'START') & (input$nextStatus != 'END'))
  if(length(tocorr) > 0){
    inp2 = input[tocorr, ] %>%
      mutate(delay = 0) %>%
      add_transit_column %>%
      left_join(obj$tables$profile.incident %>% distinct(caseID, .keep_all = T) %>% select(caseID, time_from, st_from, st_to, inctype), by = 'caseID') %>%
      mutate(incel = time_from > startTime, stmatch = (st_from == station) | (st_to == station), nxtstmatch  = (st_from == nextStation) | (st_to == nextStation))
    
    inp2$delay <- inp2 %>%
      left_join(TRANSPORT.get.transition_durations.inctype(obj), by = c('incel', 'transit', 'stmatch', 'nxtstmatch', 'inctype')) %>% pull(avg_tdn_delay)
    
    wna = is.na(inp2$delay)
    inp2$delay[wna] <- inp2[wna,] %>%
      left_join(TRANSPORT.get.transition_durations.inc(obj), by = c('incel', 'transit', 'stmatch', 'nxtstmatch')) %>% pull(avg_tdn_delay)
    
    input[tocorr, 'duration_prd'] <- input[tocorr, 'duration_prd'] + na2zero(inp2$delay)
  }
  return(input)
}

correct_durations_PB = function(input, obj){
  # Temporarily divine transition duration from history for journeys being impacted by an incident as primary service:
  # jwincs   = obj$tables$profile.incident %>% pull(caseID) %>% unique
  jwincs    = obj$tables$inc_full %>% filter(primary_flag == 1) %>% pull(caseID) %>% unique
  todivine = which(input$caseID %in% jwincs)
  
  input[todivine, c('caseID', 'station', 'nextStation')] %>% 
    left_join(obj$history %>% select(caseID, station, nextStation, duration_prd = duration) %>% distinct(caseID, station, nextStation, .keep_all = T), by = c('caseID', 'station', 'nextStation')) %>% 
    pull(duration_prd) -> dur_prd
  safe = which(!is.na(dur_prd))
  input$duration_prd[todivine[safe]] <- dur_prd[safe]
  return(input)
}

run_simulation = function(events){
  events %>% 
    mutate(duration_org = duration, startTime_org = startTime, endTime_org = endTime) %>% 
    mutate(duration = duration_prd) %>% select(-duration_prd) %>% 
    arrange(caseID, startTime) %>% group_by(caseID) %>%  
    mutate(firstTime = first(startTime), cum_dur = cumsum(duration)) %>% 
    mutate(endTime = firstTime + cum_dur) %>% 
    mutate(startTime = endTime - duration) %>% 
    select(-firstTime, -cum_dur)
}  

apply_headway_rule = function(input, obj){
  input %>% add_transit_column %>% 
    # left_join(obj$get.tdn_map(), by = 'caseID') %>% 
    # left_join(obj$get.station_map(), by = 'status') %>% 
    # left_join(obj$get.platform_map(), by = 'status') %>% 
    mutate(location = paste(station, platform, sep = '.')) %>% arrange(startTime) %>% as.data.frame -> events
  
  obj$history %>% filter(startTime < min(input$startTime)) %>% 
    mutate(location = paste(station, platform, sep = '.')) %>% 
    select(caseID, startTime, location, station) %>% 
    left_join(obj$tables$profile.station %>% select(station, headway)) %>% 
    mutate(startTime = startTime + headway) %>% arrange(startTime) %>% 
    group_by(location, station) %>% summarise(mintime = last(startTime), service = last(caseID)) %>% ungroup %>% 
    column2Rownames('location') -> occupied
  
  mintime = min(obj$history$startTime)
  occupied[paste(obj$history$station %>% na.omit, obj$history$platform %>% na.omit, sep = '.') %>% c(events$location) %>% unique %>% setdiff(c(rownames(occupied), 'NA.NA')), 'mintime'] <- mintime
  occupied$station = rownames(occupied) %>% substr(1,3)
  occupied$headway = occupied %>% left_join(obj$tables$profile.station %>% select(station, headway), by = 'station') %>% pull(headway)
  occupied = occupied[!is.na(occupied$headway),]

  ne = nrow(events)
  pb = txtProgressBar(min = 1, max = ne, style = 3)
  i  = 1
  while(i <= ne){
    ignore = T
    setTxtProgressBar(pb, i)
    if(events$status[i] != 'START'){
      if((events$startTime[i] < occupied[events$location[i], 'mintime']) & (occupied[events$location[i], 'service'] != events$caseID[i])){
        w = which((events$caseID == events$caseID[i]) & (events$endTime >  events$startTime[i] - 0.01))
        ignore = (events[w[1], 'status'] == 'START') | (events[i,'nextStatus'] == 'END')
        if(!ignore){
          opts = obj$get.transition_probabilities.tdn() %>% 
            filter(status == events[w[1], 'status'], TDN == events[w[1], 'TDN'], nextStatus != events[w[1], 'nextStatus']) %>% 
            left_join(obj$get.station_map(), by = 'status') %>% 
            left_join(obj$get.platform_map(), by = 'status') %>% 
            mutate(location = paste(station, platform, sep = '.')) %>% distinct(status, nextStatus, TDN, location)
          
          # opts = obj$get.transition_probabilities.tdn() %>% 
          #   filter(status == events[w[1], 'status'], TDN == events[w[1], 'TDN'], nextStatus != events[w[1], 'nextStatus']) %>% 
          #   left_join(obj$get.station_map() %>% rename(nextStatus = status), by = 'nextStatus') %>% 
          #   left_join(obj$get.platform_map() %>% rename(nextStatus = status), by = 'nextStatus') %>%
          #   mutate(location = paste(station, platform, sep = '.'))
          
          shifttime = T
          while((nrow(opts) > 0) & shifttime){
            obj$get.transition_probabilities.tdn() %>% 
              filter(status == opts$nextStatus[1], TDN == events[w[2], 'TDN'], nextStatus == events[w[2], 'nextStatus']) -> possible
            
            feasible = (nrow(possible) > 0) & (occupied[opts$location[1], 'mintime'] < events[w[1], 'startTime'])
            if(feasible > 0){
              events[w[1], 'nextStatus'] <- opts$nextStatus[1]
              events[w[2], 'status']     <- opts$nextStatus[1]
              events[w[2], 'location']   <- opts$location[1]
              events[w[2], 'station']    <- opts$station[1]
              events[w[2], 'platform']   <- opts$platform[1]
              
              shifttime = F
            }
            opts = opts[-1,]
          }
          if(shifttime) {
            timeshift <- difftime(occupied[events$location[i], 'mintime'], events$startTime[i], units = 'secs')
            w1 = chif(i == w[1], w, w[-1]); assert(i %in% w1)
            events[w1, 'startTime'] <- events$startTime[w1] + timeshift
            events[w, 'endTime']    <- events$endTime[w] + timeshift
            events %<>% arrange(startTime)
          }
          #i = w[1]
          #occupied_correct = events[sequence(i-1), ] %>% group_by(location) %>% summarise(mintime = last(startTime) + 120, service = last(caseID)) %>% ungroup
          #occupied[occupied_correct$location, 'mintime'] <- occupied_correct$mintime
        }
      }
      if(ignore){
        occupied[events$location[i], 'mintime'] <- chif(events$transit[i] == 0, events$endTime[i], events$startTime[i]) + occupied[events$location[i], 'headway']
        occupied[events$location[i], 'service'] <- events$caseID[i]
        i = i + 1
      }
    } else {i = i + 1}
  }
  
  return(events %>% mutate(duration = difftime(endTime, startTime, units = 'secs') %>% as.numeric))
}

attach_features = function(events, obj){
  incprofile = obj$tables$profile.incident %>% mutate(time_from = as.POSIXct(time_from), time_to = as.POSIXct(time_to))
  
  wna = which(incprofile$time_to < incprofile$time_from)
  incprofile$time_to[wna] = as.POSIXct(incprofile$incdate[wna]) + 24*3600 

  events %>% select(caseID, status, nextStatus, startTime, endTime, station, nextStation, TDN) %>% 
    add_transit_column %>% 
    mutate(dow = weekdays(as_date(startTime) %>% as.Date), hod = format(startTime, '%H')) %>% 
    left_join(incprofile %>% select(caseID, time_from, time_to, st_from, st_to, cat = incident_category_code, subcat = incident_subcategory_code, primary = primary_flag), by = 'caseID') -> tft
  
  tftinc = tft %>% filter(!is.na(primary)) %>% mutate(category = paste(cat, subcat, sep = '-')) %>% select(-cat, -subcat) %>% 
    mutate(elapsed  = difftime(startTime, time_from, units = 'secs') %>% as.numeric, resolved = (startTime > time_to)) %>% 
    mutate(active = (elapsed > 0) & (!resolved)) %>% 
    mutate(stmatch = (st_to == station) | (st_from == station), nxtstmatch = (st_to == nextStation) | (st_from == nextStation))
  
  tftinc %>% reshape2::dcast(caseID + status + nextStatus ~ category, fun.aggregate = length, value.var = 'active') %>% select(-caseID, -status, -nextStatus) %>% rowSums
  
}

