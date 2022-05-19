freq<- function(data, colName) {
  
  result <- data %>%
    group_by({{ colName }}) %>%
    tally() %>% 
    drop_na() %>% 
    mutate(percent = str_c("(", round(n / sum(n), 2) * 100, "%)")) %>% 
    arrange(desc(n))
  
  return(result)
}

# Get Proper Kable
tab <- function (.data, ...)
{
  kable_input <-
    knitr::kable(
      .data,
      digits = 3,
      format.args = list(big.mark = ",", scientific = FALSE),
      booktabs = TRUE,
      longtable = TRUE,
      align = "l",
      ...
    ) %>% column_spec(1, width = "10em")
  kable_output <-
    kableExtra::kable_styling(
      kable_input,
      full_width = FALSE,
      position = "center",
      font_size = 10,
      latex_options = c("striped", "repeat_header", "HOLD_position", "scale_down")
    )
  return(kable_output)
}

tab_two <- function (.data, ...)
{
  kable_input <-
    knitr::kable(
      .data,
      digits = 2,
      format.args = list(big.mark = ",", scientific = FALSE),
      booktabs = TRUE,
      longtable = TRUE,
      align = "l",
      ...
    ) %>% column_spec(1, width = "10em")
  kable_output <-
    kableExtra::kable_styling(
      kable_input,
      full_width = FALSE,
      position = "center",
      font_size = 10,
      latex_options = c("striped", "repeat_header", "HOLD_position", "scale_down")
    )
  return(kable_output)
}


# Get Title
title <- function(data) {
  result <- data %>%
    rename_all( ~ gsub("_", " ", .)) %>%
    rename_all(str_to_title)
  
  return(result)
}

bar<-function(data, x, y) {
  ggplot(data, aes(x = reorder({{x}}, -{{y}}), y = {{y}}, fill = {{x}})) + geom_bar(stat = "identity") +geom_text(aes(label = n), vjust = -0.3, size = 3.5)+ theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank()) + theme(plot.title = element_text(hjust=0.5))+scale_fill_brewer(palette="Set3")+ theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+theme(plot.title = element_text(face="bold"))+theme(axis.title.y = element_text(face="bold")) +
    theme(axis.title.x = element_text(face="bold"))
  
}

nb.cols <- 75
mycolors <- colorRampPalette(brewer.pal(75, "Set2"))(nb.cols)


long_bar<-function(data, x, y) {
  ggplot(data, aes(x = reorder({{x}}, {{y}}), y = {{y}}, fill = {{x}})) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = {{y}}), 
      ## make labels left-aligned
      hjust = 0, size = 2.6, position = position_dodge(0.5)
    ) +
    theme_minimal() + 
    theme(legend.position="none", panel.grid.major = element_blank()) +
    scale_fill_manual(values = mycolors)+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    coord_flip()+ theme(legend.position="none", panel.grid.major = element_blank()) + theme(plot.title = element_text(hjust=0.5))+theme(plot.title = element_text(face="bold")) +
    theme(axis.title.y = element_text(face="bold")) +
    theme(axis.title.x = element_text(face="bold"))
  
  
  
}



#Stacked Bar
stacked_bar<-function(data) {
  ggplot(data) +
    geom_bar(aes(x = reorder(var1 , `Value`), y = `Value`, fill = `Col`), stat = "identity") +
    theme_minimal() + scale_fill_brewer(palette = "Pastel2") +
    coord_flip()+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+theme(axis.text.y = element_text( size=6.5)) + theme(plot.title = element_text(hjust=0.5))+theme(plot.title = element_text(face="bold")) +
    theme(axis.title.y = element_text(face="bold")) +
    theme(axis.title.x = element_text(face="bold"))
  
}

kable_it<- function(data) {
  result <- data %>%
    adorn_totals() %>% 
    title() %>% 
    drop_na() %>% 
    tab(caption = "Respondents by Employee_type")
  
  return(result)
}


freq_table<- function(data) {
  result <- data %>%
    lapply(table) %>%
    lapply(as.data.frame) %>%
    Map(bind_cols, var = names(data), .) %>%
    bind_rows() %>%
    select(Var1, Freq) %>%
    arrange(desc(`Freq`)) %>% 
    clean_names()
  
  return(result)
}

pivot_all<-function(data) {
  result <- data %>% 
    pivot_longer(cols = -var1, names_to = "Col", values_to = "Value")
  return(result)
}

# Age Stas
current_age_ds<- function(data, colName) {
  
  result <- data %>%
    group_by({{ colName }}) %>%
    summarize(current_age = mean(current_age),
              median_age = median(current_age),
              n=n(),
              sd=sd(current_age),
              se=sd/sqrt(n)
    ) %>% 
    arrange(desc(n)) %>% 
    title()
  
  return(result)
}


age_ds_overall<- function(data, colName) {
  
  result <- data %>%
    summarize(average_age = mean(age),
              median_age = median(age),
              n=n(),
              sd=sd(age),
              se=sd/sqrt(n)
    ) %>% 
    arrange(desc(n)) %>% 
    title()
  
  return(result)
}

duration_ds<- function(data, colName) {
  
  result <- data %>%
    group_by({{ colName }}) %>%
    summarize(average_duration = mean(duration_lab_to_procedure),
              median_age = median(duration_lab_to_procedure),
              n=n(),
              sd=sd(duration_lab_to_procedure),
              se=sd/sqrt(n)
    ) %>% 
    arrange(desc(n)) %>% 
    title()
  
  return(result)
}


duration_ld <- function(data, colName) {
  result <- data %>%
    group_by({
      {
        colName
      }
    }) %>%
    summarize(
      average_duration = mean(duration_lab_to_diagnosis),
      median_age = median(duration_lab_to_diagnosis),
      n = n(),
      sd = sd(duration_lab_to_diagnosis),
      se = sd / sqrt(n)
    ) %>%
    arrange(desc(n)) %>%
    title()
  
  return(result)
}

# Age Stas
age_ds<- function(data, colName) {
	
	result <- data %>%
		group_by({{ colName }}) %>%
		summarize(average_age = mean(age),
							median_age = median(age),
							n=n(),
							sd=sd(age),
							se=sd/sqrt(n)
		) %>% 
		arrange(desc(n)) %>% 
		title()
	
	return(result)
}



age_ds_overall<- function(data, colName) {
	
	result <- data %>%
		summarize(average_age = mean(age),
							median_age = median(age),
							n=n(),
							sd=sd(age),
							se=sd/sqrt(n)
		) %>% 
		arrange(desc(n)) %>% 
		title()
	
	return(result)
}

age_labs <- c(paste(seq(0, 95, by = 10), seq(0 + 10 - 1, 100 - 1, by = 10),
										sep = "-"), paste(100, "+", sep = ""))


bmi_ds<- function(data, colName) {
	
	result <- data %>%
		drop_na(bmi) %>% 
		group_by({{ colName }}) %>%
		summarize(average_bmi = mean(bmi),
							median_bmi = median(bmi),
							n=n(),
							sd=sd(bmi),
							se=sd/sqrt(n)
		) %>% 
		arrange(desc(n)) %>% 
		title()
	
	return(result)
}

freq_tab <- function(data) {
	result <- data %>%
		imap(
			~ table(.x) %>%
				enframe(name = "key") %>%
				mutate(var = .y,
							 freq = round(prop.table(value) * 100, 2)) %>% relocate(var)
		) %>% bind_rows() %>%
		filter(key == "1") %>%
		select(-key)
	return(result)
}



