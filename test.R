ggplot(rawdata, aes(x=Product, y=Total, fill=Total)) +
  geom_bar(stat='identity') +
  theme_bw() +
  transition_states(
    year,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out') +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = 'Drafting Assistant Usage: {closest_state}', x="", y="")

anim_save("da-barplot-transition.gif")