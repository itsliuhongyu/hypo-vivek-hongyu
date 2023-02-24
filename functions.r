library(tidyverse)
library(ggforce)
library(ggtext)
library(ggrepel)
library(emojifont)
library(ggbeeswarm)
load.emojifont('EmojiOne.ttf')


# Create a custom facet labeller function
facet_labeller <- function(variable, value) {
    if (class(value) == "Date") {
        return(strftime(value, format="%m/%d"))
    } else {
        return(value)
    }
}

# plot 1
plot_type_1 <- function(q1) {

    ggplot(q1, aes(x=0, y=0, color=choice_id)) + 
        geom_point(data=q1, 
                   position = position_jitternormal(sd_x = 0.3, sd_y = 0.3), 
                   size=3)+
        # geom_label_repel(data=q1 %>% filter(choice_id<0)) +
        xlim(-1,1) + ylim(-1,1) +
        scale_color_gradient2(midpoint = 0,
                             high = 'green2',
                             mid = 'yellow2',
                             low = 'red3',
                             na.value = 'gray95') +
        facet_grid(emoji~date, scales='free', switch='y', labeller = facet_labeller) +
        labs(title="Hi There! How are you doing?") +
        theme_minimal()+
        theme(
          plot.background = element_rect(fill = "#FAF9F6", color=NA),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(family="EmojiOne", size=20) ,
          axis.title.y=element_text(family="EmojiOne", size=20),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          strip.text.y.left= element_text(family="EmojiOne", size=20, angle=0),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          legend.position = 'none',
          plot.title = element_text(size=22),
          strip.text.x = element_text(size = 10)
        )
}

plot_type_2 <- function(q2) {

    ggplot(q2, aes(x=date, y=choice_id)) +
        geom_violin() + geom_jitter(alpha=.2)  + stat_summary(fun = "mean", colour = "red", size = 2, geom = "point") +
        geom_hline(yintercept=5, color='red', alpha=.5) +
        scale_x_date(breaks=q2$date %>% unique(), date_labels='%a %m/%d') +
        scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits=c(0,10))  +
        theme(
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.grid.major.y=element_blank(),
            panel.grid.minor.y=element_blank(),
            panel.grid.major.x=element_blank(),
            panel.grid.minor.x=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_markdown(),
            plot.title = element_text(size=22)
        ) + 
        labs(title="And how are you feeling about the workload?",
             subtitle="Average class response on W/F @ 1pm") + 
        facet_wrap(~date,scales='free_x',nrow=1, labeller = facet_labeller)
}