require(plotly)
library(data.table)
library(sqldf)

Sys.setenv("plotly_username"="shubh24")
Sys.setenv("plotly_api_key"="Jcgrh6kwxqOMZ3PerBKb")

font <- list(
  family = "Roboto",
  size = 18,
  color = "#7f7f7f"
)

get_minutes = function(vec){
  date_split = as.data.frame(do.call(rbind, strsplit(as.character(vec), ":")))
  return (as.numeric(as.character(date_split$V1))*60 + as.numeric(as.character(date_split$V2)) + as.numeric(as.character(date_split$V3))/60.0)
}

df = read.csv("marathon_results_2016.csv")
df$time_taken = get_minutes(df$Official.Time)

quals = read.csv("qualifying.csv")
quals$mens_limit = get_minutes(quals$Men)
quals$women_limit = get_minutes(quals$Women)
quals$Men = NULL
quals$Women = NULL

plot_positions_overall = function(){
  xaxis <- list(
    title = "Time(in minutes)",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Ranking (Gender-wise)",
    titlefont = font
  )
  
  p = plot_ly(df[df$time_taken < 400,], x = ~time_taken, y = ~Gender, type = "scatter", mode = "markers", color = ~M.F, colors = c("pink", "violet")) %>%
      layout(yaxis = yaxis, xaxis = xaxis, title = "Ranking against Timing")

  return (p)
  
}

plot_positions_winners = function(){
  xaxis <- list(
    title = "Time(in minutes)",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Ranking (Gender-wise)",
    titlefont = font
  )
  
  p = plot_ly(df[df$Gender < 100,], x = ~time_taken, y = ~Gender, type = "scatter", mode = "markers", color = ~M.F, colors = c("pink", "violet")) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Ranking against Timing (Top 100)")
  
  return (p)
  
}

plot_positions_division = function(){
  xaxis <- list(
    title = "Time(in minutes)",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Ranking (Division-wise)",
    titlefont = font
  )
  
  p = plot_ly(df[df$time_taken < 400,], x = ~time_taken, y = ~Division, type = "scatter", mode = "markers", color = ~Overall) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Ranking against Timing")
  
  return (p)
  
}

plot_positions_age = function(){
  xaxis <- list(
    title = "Age of athlete",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Time(in minutes)",
    titlefont = font
  )
  
  p = plot_ly(df[df$time_taken < 400,], x = ~Age, y = ~time_taken, type = "box", mode = "markers", color = ~M.F, colors = c("pink", "violet")) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Boxplot of Timings against Age")
  
  return (p)
  
}

plot_world_choropleth = function(top_n){
  
  l <- list(color = toRGB("grey"), width = 0.5)
  
  g <- list(
    showframe = FALSE,
    showcoastlines = TRUE,
    projection = list(type = 'Mercator')
  )
  
  country_agg = as.data.frame(table(df$Country[df$Gender <= top_n]))
  
  p <- plot_geo(country_agg) %>%
      add_trace(
        z = ~Freq, color = ~Freq, colors = 'Blues',
        text = ~Var1, locations = ~Var1, marker = list(line = l)
      ) %>%
      colorbar(title = 'Number of Athletes') %>%
      layout(
        title = paste0("Heatmap of Athletes within the Top ", top_n, " (both genders)"),
        geo = g
      )
  
  return(p)
  
}

get_quals_discipline = function(){
    
    sub_df = df[, c("Bib", "M.F", "Age", "time_taken")]
    
    df_quals = sqldf("
      SELECT *
      FROM sub_df INNER JOIN quals
      ON sub_df.Age >= quals.AgeMin and sub_df.Age <= quals.AgeMax
    ")
    
    df_quals$within_qualifying[df_quals$M.F == "M"] = as.factor(df_quals$time_taken[df_quals$M.F == "M"] <= df_quals$mens_limit[df_quals$M.F == "M"])  
    df_quals$within_qualifying[df_quals$M.F == "F"] = as.factor(df_quals$time_taken[df_quals$M.F == "F"] <= df_quals$women_limit[df_quals$M.F == "F"])  
    
    df_quals$dummy = 1
    df_quals$AgeCategory = paste0(df_quals$AgeMin, "-", df_quals$AgeMax)
    
    df_quals_agg = aggregate(dummy ~ AgeCategory + M.F + within_qualifying, data = df_quals, FUN = sum)
    
    age_cats = unique(df_quals_agg$AgeCategory)
    final_df = data.frame()
    
    for (i in 1:length(age_cats)){
            
      for (j in c("M","F")){
        age_ratio_df = df_quals_agg[df_quals_agg$AgeCategory == age_cats[i] & df_quals_agg$M.F == j,]
        
        if (nrow(age_ratio_df[age_ratio_df$within_qualifying == 2,]) > 0){
          qual_perc = round(100*age_ratio_df$dummy[age_ratio_df$within_qualifying == 2]/sum(age_ratio_df$dummy), 2)
        }
        else{
          qual_perc = 0  
        }
        
        final_df = unique(rbind(final_df, cbind(age_ratio_df[, c("AgeCategory", "M.F")], qual_perc)))
      }      
    }
    
    xaxis <- list(
      title = "Category of Athlete",
      titlefont = font
    )
    
    yaxis <- list(
      title = "% of Athletes",
      titlefont = font
    )
    p = plot_ly(final_df, x = ~AgeCategory, y = ~qual_perc, type = "bar", mode = "markers", color = ~M.F, colors = c("pink", "violet")) %>%
      layout(yaxis = yaxis, xaxis = xaxis, title = "% Athletes with better Timing than Qualifying Limits")
    
    return(p)    
}

data.table::data.table(df[df$Gender <= 10 & df$M.F == "M",  c("Name", "Age", "Country", "Pace", "Official.Time")])
data.table::data.table(df[df$Gender <= 10 & df$M.F == "F",  c("Name", "Age", "Country", "Pace", "Official.Time")])

get_pace_discipline = function(top_n, gender_separated){
  split_df = df[df$Gender <= top_n, c("Name", "M.F", "X5K", "X10K", "X15K", "Half", "X25K", "X30K", "X35K", "X40K", "Official.Time")]
  
  for (i in 3:11){
    split_df[, i] = get_minutes(split_df[, i])
  }
  
  split_df[, "X42K"] = split_df$Official.Time - split_df$X40K
  split_df[, "Pace"] = split_df$Official.Time/42.195
  split_df$Official.Time = NULL
  
  for (i in 10:4){
    split_df[, i] = split_df[, i] - split_df[, i-1]
    
    if (i == 7){
      split_df[, i] = 100*(split_df[, i] - split_df$Pace*3.9025)/(split_df$Pace*3.9025)
    }
    else if (i == 6){
      split_df[, i] = 100*(split_df[, i] - split_df$Pace*6.0975)/(split_df$Pace*6.0975)
    }
    else{
      split_df[, i] = 100*(split_df[, i] - split_df$Pace*5)/(split_df$Pace*5)
    }
  }
  
  split_df[, 11] = 100*(split_df[, 11] - split_df$Pace*2.195)/(split_df$Pace*2.195)
  split_df[, 3] = 100*(split_df[, 3] - split_df$Pace*5)/(split_df$Pace*5)
  
  split_df$Pace = NULL
  
  molten_split_df = melt(split_df, id.vars = c("Name", "M.F"))
  
  xaxis <- list(
    title = "Distance splits ",
    titlefont = font
  )
  
  yaxis <- list(
    title = "% Deviation from Mean Pace",
    titlefont = font
  )
  
  if (gender_separated == 1){
    molten_split_df = aggregate(value ~ M.F + variable, data = molten_split_df, FUN = mean)  
    
    p = plot_ly(molten_split_df, x = ~variable, y = ~value, type = "scatter", mode = "lines", color = ~M.F, colors = c("pink", "violet")) %>%
      layout(yaxis = yaxis, xaxis = xaxis, title = paste0("Pace Discipline of Top ", top_n, " athletes(both genders)"))
  }

  else{
    p = plot_ly(molten_split_df, x = ~variable, y = ~value, type = "scatter", mode = "markers", color = ~Name) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = paste0("Pace Discipline of Top ", top_n, " athletes(both genders)"))
  }
  return(p)
}

get_relative_split_discipline = function(top_n){
  
  split_df = df[df$Gender <= top_n, c("Name", "M.F", "X5K", "X10K", "X15K", "Half", "X25K", "X30K", "X35K", "X40K", "Official.Time")]
  
  for (i in 3:11){
    split_df[, i] = get_minutes(split_df[, i])
  }
  
  split_df[, "X42K"] = split_df$Official.Time - split_df$X40K
  
  split_df$Official.Time = NULL
  
  for (i in 10:4){
    split_df[, i] = split_df[, i] - split_df[, i-1]
  }
  
  split_df_m = split_df[split_df$M.F == "M",]
  split_df_f = split_df[split_df$M.F == "F",]
  
  winner_m = split_df_m[1, ]
  winner_f = split_df_f[1, ]
  others_m = split_df_m[2:10,]
  others_f = split_df_f[2:10,]
  
  avg_m = as.data.frame(cbind("M", mean(others_m$X5K), mean(others_m$X10K), mean(others_m$X15K), mean(others_m$Half), mean(others_m$X25K), mean(others_m$X30K), mean(others_m$X35K), mean(others_m$X40K), mean(others_m$X42K), "Others in Top 10"))
  avg_f = as.data.frame(cbind("F", mean(others_f$X5K), mean(others_f$X10K), mean(others_f$X15K), mean(others_f$Half), mean(others_f$X25K), mean(others_f$X30K), mean(others_f$X35K), mean(others_f$X40K), mean(others_f$X42K), "Others in Top 10"))
  
  final_df = data.frame(winner_m[, 2:11])
  final_df$position = "Winner"
  
  names(avg_m) = names(final_df)
  final_df= rbind(final_df, avg_m)
  
  final_df$M.F = NULL
  
  molten_final_df = melt(final_df, id.vars = c("position"))
  molten_final_df$value = as.numeric(molten_final_df$value)
  
  xaxis <- list(
    title = "Distance splits ",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Time taken in split (in minutes)",
    titlefont = font
  )
  
  p1 = plot_ly(molten_final_df, x = ~variable, y = ~value, type = "scatter", mode = "lines", color = ~position, colors = c("violet", "red")) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Relative Pace Discipline of Winner w.r.t Top 10 (Mens)")
  
  final_df = data.frame(winner_f[, 2:11])
  final_df$position = "Winner"
  
  names(avg_f) = names(final_df)
  final_df= rbind(final_df, avg_f)

  final_df$M.F = NULL
  
  molten_final_df = melt(final_df, id.vars = c("position"))
  molten_final_df$value = as.numeric(molten_final_df$value)

  p2 = plot_ly(molten_final_df, x = ~variable, y = ~value, type = "scatter", mode = "lines", color = ~position, colors = c("violet", "red")) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = "Relative Pace Discipline of Winner w.r.t Top 10 (Women)")
  
  return(list(p1, p2))
}

slow_starts = function(top_n){
  
  split_df = df[df$Gender <= top_n, c("Name", "M.F", "X5K", "Gender", "time_taken")]
  
  split_df[, "Pace"] = split_df$time_taken/42.195
  # split_df$time_taken = NULL
  
  split_df$X5K = get_minutes(split_df$X5K)
  
  split_df$X5K = 100*(split_df$X5K - split_df$Pace*5)/(split_df$Pace*5)
  # split_df$Pace = NULL
  
  
  xaxis <- list(
    title = "%Deviation from Mean (0 - 5K)",
    titlefont = font
  )
  
  yaxis <- list(
    title = "Ranking (gender-wise)",
    titlefont = font
  )
  
  p = plot_ly(split_df, x = ~X5K, y = ~Gender, type = "scatter", mode = "markers", color = ~M.F) %>%
    layout(yaxis = yaxis, xaxis = xaxis, title = paste0("Ranking against First 5K split(Top ", top_n, " athletes)"))

    return (p)
}

plotly_IMAGE(p, format = "png", out_file = "slow_start")
