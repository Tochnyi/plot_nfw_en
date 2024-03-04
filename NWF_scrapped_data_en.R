#Written by @UlysseColonna

# Main inspiration for the graph: https://r-graph-gallery.com/web-stacked-area-chart-inline-labels.html

library(tidyverse)
library(janitor)
library(lubridate)
library(rvest)
library(yahoofinancer)
library(ggstream)
library(showtext)
library(ggtext)
library(png)


### GETTING THE FONTS AND COLORS
font <- "Josefin Sans"
font2 <- "Open Sans"

# Use the font_add_google() function to load fonts from the web
font_add_google(family=font, font, db_cache = FALSE)
font_add_google(family=font2, font2, db_cache = FALSE)

sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "Font Awesome 6 Brands-Regular-400.otf")
showtext::showtext_auto()


theme_set(theme_minimal(base_family = font2, base_size = 3))

bg <- "white"
txt_col <- "black"

showtext_auto(enable = TRUE)

#Color palette
pal=c("darkgrey", "gold",
      "red",
      "blue",
      "lightblue",
      "cyan",
      'darkgreen')

order <- c("rub","gold","cny","eur","jpy", "gbp", "usd"  )


currencies_ru = c("доллары США","евро", "фунты стерлингов", "японские иены","китайские юани", "золото в обезличенной форме", "рубли") 

nwf_url = "https://minfin.gov.ru/ru/document?id_4=93488-dannye_o_dvizhenii_sredstv_i_rezultatakh_upravleniya_sredstvami_fonda_natsionalnogo_blagosostoyaniya"
nwf_html = read_html(nwf_url)
nwf_tbl = nwf_html  %>% html_elements("table") %>%   html_table() %>% .[[1]]

Sys.setlocale(,"ru_RU")

nwf_df = nwf_tbl %>% 
  filter( str_detect( `№\r\n\t\t\tп/п`, '4\\.[0-9]\\.' )   ) %>% 
  filter(  str_detect( Показатель, currencies_ru %>% glue::glue_collapse("|")) ) %>% 
  select(-1) %>% 
  pivot_longer( !Показатель, names_to = "date") %>% 
  mutate( 
    date = date %>% 
      str_extract_all("([:alnum:]|[:blank:]){1,}") %>% 
      tolower() %>% 
      str_remove_all("(?<=\\w{3})\\w{1,}(?=(\\s[:digit:]{4}))") %>%   
      paste('1' , .) %>% 
      lubridate::dmy(locale = "ru_RU")  %>% 
      lubridate::ceiling_date(unit = "month") ,
    asset = Показатель %>% 
          str_replace_all(currencies_ru[1] , "usd") %>% 
          str_replace_all(currencies_ru[2] , "eur") %>% 
          str_replace_all(currencies_ru[3] , "gbp") %>% 
          str_replace_all(currencies_ru[4] , "jpy") %>% 
          str_replace_all(currencies_ru[5] , "cny") %>% 
          str_replace_all(currencies_ru[6] , "gold") %>% 
          str_replace_all(currencies_ru[7] , "rub") %>% 
          str_extract("[:alpha:]{1,}") ) %>% 
  select(date, asset, value) %>% 
  filter( value != "-") %>% 
  mutate( value = value %>% str_extract("([:digit:]|\\.){1,}")) %>% 
  filter( date >= dmy("01012021")) %>%
  arrange(date, asset)
  
Sys.setlocale(,"en_US")

mindt = min(nwf_df$date)
maxdt = max(nwf_df$date)
maxdt_chr = maxdt %>% format("%b %Y")
maxdt_num = maxdt %>% format("%y%m")

# Convert to USD
list_curr = c("jpy", "gbp", "eur", "rub", 'cny')

for (curr_i in list_curr ){
  nwf_df = currency_converter(curr_i,'USD', mindt, maxdt, interval= '1mo') %>% 
    as_tibble(  ) %>% 
    mutate( date = (date + days(2)) %>% lubridate::as_date() %>% floor_date("month") ) %>% 
    select(date, open) %>% 
    rename( !!paste0(curr_i, "_conv_rate") := "open") %>% 
    right_join( nwf_df, by = "date")
}

gold <-Ticker$new('GC=F') 
nwf_df = gold$get_history(start= mindt,
                            end=(maxdt + days(10)),
                            interval= '1d') %>% 
  as_tibble %>% 
  mutate( date = (date + hours(5)) %>% lubridate::as_date()) %>% 
  mutate( monthdt = month(date), yeardt = year(date)) %>% 
  group_by(monthdt, yeardt) %>%
  filter(date == min(date)) %>% 
  ungroup %>% 
  mutate( date = date %>% floor_date("month")) %>% 
  select(date, open) %>% 
  rename( gold_in_usd = "open") %>% 
  right_join( nwf_df, by = "date")

lg_nwf_df = nwf_df %>% 
  mutate( conv_rate = case_when( asset == "eur" ~ eur_conv_rate,
                                 asset == "usd" ~ 1,
                                 asset == "cny" ~ cny_conv_rate,
                                 asset == "gbp" ~ gbp_conv_rate,
                                 asset == "jpy" ~ jpy_conv_rate, 
                                 asset == "rub" ~ rub_conv_rate, 
                                 asset == "gold" ~ (gold_in_usd * 32150.7465)/10^9 ) ,
          value = value %>% as.numeric(),
          value_in_usd = value * conv_rate ) %>% 
  select(date, asset, value_in_usd)


# Get breaks details

breaks_df = lg_nwf_df %>% 
  mutate( date2 = date %>% format("%b %Y")) %>% 
  filter( date2 %>% str_detect("Jan|Jul")) 

breaks_locs = breaks_df$date %>% 
  unique()  

breaks_names = breaks_df$date2 %>% 
  unique()

# Get data for the side notes

most_recent_df = lg_nwf_df %>% filter(date == max(date))
latest_date = max(most_recent_df$date) + days(10)
cny_last_val = most_recent_df %>% filter(asset == "cny") %>%  .$value_in_usd %>% round(2)
gold_last_val_added = most_recent_df %>% filter(asset %in% c("gold","cny")) %>%  .$value_in_usd %>% sum %>% round(3)
gold_last_val = most_recent_df %>% filter(asset == "gold") %>%  .$value_in_usd %>% round(2)

earliest_recent_df = lg_nwf_df %>% filter(date == min(date))
earliest_date = min(earliest_recent_df$date) - months(5)
usd_first_val = earliest_recent_df %>% filter(asset == "usd") %>%  .$value_in_usd %>% round(2)
gbp_first_val_added = earliest_recent_df %>% filter(asset %in% c("usd","gbp")) %>%  .$value_in_usd %>% sum %>% round(3)
gbp_first_val = earliest_recent_df %>% filter(asset == "gbp") %>%  .$value_in_usd %>% round(2)
eur_first_val_added = earliest_recent_df %>% filter(asset %in% c("usd","gbp", "eur")) %>%  .$value_in_usd %>% sum %>% round(3)
eur_first_val = earliest_recent_df %>% filter(asset == "eur") %>%  .$value_in_usd %>% round(2)

for (i in 1:length(breaks_locs)){
  val_break_tempo = lg_nwf_df %>% filter(date == breaks_locs[i]) %>% .$value_in_usd %>% sum
  assign(paste0('val_break_',i), val_break_tempo) }



# Prep TOSCHNYI markings
ggithub_icon <- "&#xf099"
github_username <- "@tochnyi"

social_caption <- glue::glue("**Data:** minfin.gov.ru<br>",
                             "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: #000000'>{github_username}</span>"
)


#### PLOT
size_font_side_ticks = 5


plot <- lg_nwf_df %>% 
  # filter( asset != "usd") %>% 
  mutate(asset = factor(asset, levels=order)) %>%
  ggplot(aes(date, value_in_usd, fill = asset, label = asset, color = asset)) +
  geom_area() +
  
  #Title
  annotate("text", x = ymd(20230901), y = 138,
           label = paste0("Liquid assets portfolio of\nRussia's National\nWealth Fund"),
           size=10,
           lineheight=1,
           fontface="bold", family=font,
           color="black") +
  
  #Gold last
  annotate("text", x = latest_date, y = gold_last_val_added,
           label =  paste0("Gold $", gold_last_val, "bn"),
           hjust=0,
           size=size_font_side_ticks,
           lineheight=.8,
           fontface="bold", family=font2,
           color=pal[2]) +
  #CNY last
  annotate("text", x = latest_date, y = cny_last_val,
           label = paste0("CNY $", cny_last_val,"bn"),
           hjust=0,
           size=size_font_side_ticks,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[3]) +
  
  #EUR first
  annotate("text", x = earliest_date, y = eur_first_val_added,
           label = paste0("EUR $", eur_first_val,"bn"),
           hjust=0,
           size=size_font_side_ticks,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[4]) +
  
  #GBP first
  annotate("text", x = earliest_date, y = gbp_first_val_added,
           label = paste0("GBP $", gbp_first_val,"bn"),
           hjust=0,
           size=size_font_side_ticks,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[6]) +
  
  #USD first
  annotate("text", x = earliest_date, y = usd_first_val,
           label = paste0("USD $", usd_first_val,"bn"),
           hjust=0,
           size=size_font_side_ticks,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[7]) +
  
  ## Vertical segments
  geom_segment(aes(x = breaks_locs[1]+days(1), y = val_break_1, xend = breaks_locs[1]+days(1), yend = val_break_1+3.5),color="black") +
  geom_point(aes(x = breaks_locs[1]+days(1), y = val_break_1+3.5),color="black") +
  annotate("text", x = breaks_locs[1]+days(1), y = val_break_1+5.6,
           label = paste0("$", round(val_break_1,2),"bn"),
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",family=font2,
           color=txt_col) +
  
  geom_segment(aes(x = breaks_locs[2], y = val_break_2, xend = breaks_locs[2], yend = val_break_2+3.5),color="black") +
  geom_point(aes(x = breaks_locs[2], y = val_break_2+3.5),color="black") +
  annotate("text", x = breaks_locs[2], y = val_break_2+5.6,
           label = paste0("$", round(val_break_2,2),"bn"),
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",family=font2,
           color=txt_col) +
  
  geom_segment(aes(x = breaks_locs[3], y = val_break_3, xend = breaks_locs[3], yend = val_break_3+3.5),color="black") +
  geom_point(aes(x = breaks_locs[3], y = val_break_3+3.5),color="black") +
  annotate("text", x = (breaks_locs[3]), y = val_break_3+5.6,
           label = paste0("$", round(val_break_3,2),"bn"),
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",family=font2,
           color=txt_col) +
  
  geom_segment(aes(x = breaks_locs[4], y = val_break_4, xend = breaks_locs[4], yend = val_break_4+3.5),color="black") +
  geom_point(aes(x = breaks_locs[4], y = val_break_4+3.5),color="black") +
  annotate("text", x = breaks_locs[4], y = val_break_4+5.6,
           label = paste0("$", round(val_break_4,2),"bn"),
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",family=font2,
           color=txt_col) +
  
  geom_segment(aes(x = breaks_locs[5], y = val_break_5, xend = breaks_locs[5], yend = val_break_5+3.5),color="black") +
  geom_point(aes(x = breaks_locs[5], y = val_break_5+3.5),color="black") +
  annotate("text", x = (breaks_locs[5]+days(23)), y = val_break_5+5.6,
           label = paste0("$", round(val_break_5,2),"bn"),
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",family=font2,
           color=txt_col) +
  
  geom_segment(aes(x = breaks_locs[6], y = val_break_6, xend = breaks_locs[6], yend = val_break_6+3.5),color="black") +
  geom_point(aes(x = breaks_locs[6], y = val_break_6+3.5),color="black") +
  annotate("text", x = breaks_locs[6], y = val_break_6+5.6,
           label = paste0("$", round(val_break_6,2),"bn"),
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",family=font2,
           color=txt_col) +
  
  geom_segment(aes(x = breaks_locs[7], y = val_break_7, xend = breaks_locs[7], yend = val_break_7+3.5),color="black") +
  geom_point(aes(x = breaks_locs[7], y = val_break_7+3.5),color="black") +
  annotate("text", x = breaks_locs[7]+days(25), y = val_break_7+5.6,
           label = paste0("$", round(val_break_7,2),"bn"),
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",family=font2,
           color=txt_col) +
  
  
  geom_curve(aes(x = ymd(20221227), 
                 xend = (ymd(20221227)+days(30)),
                 y = (95), yend = 101),
             curvature = -.4) +
  annotate("text", 
           x = (ymd(20221227)+days(104)),
           y = (102),
           label = '"Replenishment of the NWF could\n begin as early as January"',
           size = 3,
           lineheight = .9) +
  
  geom_curve(aes(x = ymd(20210715), 
                 xend = (ymd(20210715)+days(30)),
                 y = (125), yend = 116),
             curvature = -.4) +
  annotate("text", 
           x = (ymd(20210715)-days(32)),
           y = (125.5),
           label = 'USD dropped\nfrom NWF',
           size = 3,
           lineheight = .9) +
  
  geom_curve(aes(x = ymd(20220728),
                 xend = (ymd(20220728)+days(40)),
                 y = (141.5), yend = 148),
             curvature = -.4) +
  annotate("text", 
           x = (ymd(20220728)+days(82)),
           y = (148.7),
           label = 'The NWF to drop\nGBP and JPY',
           size = 3,
           lineheight = .9) +
  
  geom_curve(aes(x = ymd(20231215),
                 xend = (ymd(20231215)+days(40)),
                 y = (67), yend = 75),
             curvature = -.4) +
  annotate("text", 
           x = (ymd(20231215)+days(78)),
           y = (75),
           label = 'Net $20bn\nwithdrawl to\ncover deficit',
           size = 3,
           lineheight = .9) +
  
  geom_curve(aes(x = ymd(20221207),
                 xend = (ymd(20221207)+days(40)),
                 y = (120), yend = 114),
             curvature = 0) +
  annotate("text", 
           x = (ymd(20221204)+days(78)),
           y = (112),
           label = 'Net $37bn\nwithdrawl to\ncover deficit',
           size = 3,
           lineheight = .9) +
  
  geom_curve(aes(x = ymd(20220507),
                 xend = (ymd(20220515)-days(50)),
                 y = (120), yend = 130),
             curvature = -.4) +
  annotate("text", 
           x = (ymd(20220515)-days(85)),
           y = (134),
           label = "$26bn transfered from\nfederal budget as additional\noil and gas revenues for 2021",
           size = 3,
           lineheight = .9) +
  
  
  #Color scale
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  scale_x_continuous(breaks=breaks_locs,labels = breaks_names) +
  scale_y_continuous(expand = c(0,0)) +
  
  #Last customization
  coord_cartesian(clip = "off") +
  xlab("") +
  ylab("") +
  labs(caption =   labs(caption = social_caption) 
  ) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    axis.line.x = element_line(linewidth = .75),
    panel.grid = element_blank(),
    axis.text.y=element_blank(),
    axis.text.x = element_text(color=txt_col, size=10,margin = margin(5,0,0,0)),
    plot.margin = margin(40,80,10,10),
    legend.position = "none",
    plot.caption = element_markdown(hjust=0, margin=margin(10,0,0,0), size=8, color=txt_col, lineheight = 1.2),
  )


long_logo <- readPNG(paste0(getwd(),'/Documents/TOCHNY/Research/NWF/Tochnyi_Logo_copy.png'))

plot2 = plot + 
  annotation_raster(long_logo, ymin = 130, ymax= 150, xmin = ymd(20200801),xmax = ymd(20210901))

plot2

output = paste0(getwd(),"/Documents/TOCHNY/Research/NWF/plot_liquid_nfw_2109to",maxdt_num,".pdf")

ggsave(filename = output,
       plot = plot2,
       height = 22,
       width = 36,
       units = "cm")









