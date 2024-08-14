library(data.table)
library(ggplot2)
library(viridis)
library(ggrepel) ## For the male/female labels

# Data setup --------------------------------------------------------------

plots <- data.table(
  set = rep(1:10, each = 4),
  gender = c("male", "female"),
  participant = rep(c("yes", "no"), each = 2),
  dv = 0
)

# There are ten sets, distinguished by the main effects of gender,
# summer school attendance, and whether or not there is an interaction present.

# The content of the set is listed under the set number
# [Main effect gender], [main effect summer school], [interaction]


# Set 1
# Males +, Part +, no
plots[set == 1 & gender == "male" & participant == "yes",   dv := 9]
plots[set == 1 & gender == "male" & participant == "no",    dv := 6]
plots[set == 1 & gender == "female" & participant == "yes", dv := 6]
plots[set == 1 & gender == "female" & participant == "no",  dv := 3]


# Set 2
# No, no, yes
plots[set == 2 & gender == "male"   & participant == "yes", dv := 9]
plots[set == 2 & gender == "male"   & participant == "no",  dv := 3]
plots[set == 2 & gender == "female" & participant == "yes", dv := 3]
plots[set == 2 & gender == "female" & participant == "no",  dv := 9]


# Set 3
# No, Part +, yes
plots[set == 3 & gender == "male"   & participant == "yes", dv := 9]
plots[set == 3 & gender == "male"   & participant == "no",  dv := 3]
plots[set == 3 & gender == "female" & participant == "yes", dv := 6]
plots[set == 3 & gender == "female" & participant == "no",  dv := 6]


# Set 4
# No, Part -, yes
plots[set == 4 & gender == "male"   & participant == "yes", dv := 6]
plots[set == 4 & gender == "male"   & participant == "no",  dv := 6]
plots[set == 4 & gender == "female" & participant == "yes", dv := 3]
plots[set == 4 & gender == "female" & participant == "no",  dv := 9]


# Set 5
# Male +, no, no
plots[set == 5 & gender == "male",   dv := 9]
plots[set == 5 & gender == "female", dv := 3]


##### Note: A slight difference was introduced to prevent overlap in the plot
# Set 6
# No, no, no
plots[set == 6, dv := c(5.8, 6.2, 5.8, 6.2)]


# Set 7
# Male +, no, yes
plots[set == 7 & gender == "male"   & participant == "yes", dv := 6]
plots[set == 7 & gender == "male"   & participant == "no",  dv := 9]
plots[set == 7 & gender == "female" & participant == "yes", dv := 6]
plots[set == 7 & gender == "female" & participant == "no",  dv := 3]


# Set 8
# Female +, no, yes
plots[set == 8 & gender == "male"   & participant == "yes", dv := 6]
plots[set == 8 & gender == "male"   & participant == "no",  dv := 3]
plots[set == 8 & gender == "female" & participant == "yes", dv := 6]
plots[set == 8 & gender == "female" & participant == "no",  dv := 9]


# Set 9
# Female +, no, no
plots[set == 9 & gender == "male",   dv := 3]
plots[set == 9 & gender == "female", dv := 9]


##### Note: A slight difference was introduced to prevent overlap in the plot
# Set 10
# No, yes, no
plots[set == 10 & participant == "yes", dv := 9]
plots[set == 10 & participant == "no",  dv := 3]
plots[set == 10 & gender == "male", dv := dv - .2]
plots[set == 10 & gender == "female", dv := dv + .2]


plots[, gender := as.factor(gender)]
plots[, participant := as.factor(participant)]


# Plot --------------------------------------------------------------------

# Couldn't get this to work. :(
# jitterer <- position_jitter(width = 0, height = .8, seed = 200)

ggplot(plots, aes(y = dv, x = participant, color = gender, group = gender)) +
  # geom_line(size = 2, position = jitterer()) +  # Couldn't get the jitter to function the same 
  geom_line(size = 2) +
  # geom_point(size = 5, position = jitterer()) + # across these two geoms, so just made them different values
  geom_point(size = 5) +
  geom_label_repel(data = plots[participant == "no"], # only label the first point
            aes(label = gender),
            show.legend = F,
            force_pull = .75, # Don't pull so hard towards the point
            point.padding = .25, # Labels further away from points
            color = "black",
            )+
  facet_wrap(~ set, ncol = 3, scales = "free_x") + # free_x keeps x-axis label on each facet for easy printing
  scale_y_continuous(limits = c(0, 12))+
  scale_color_viridis_d(end = .75, option = "D") + # end is set to .75 to avoid printing the yellow
  theme_minimal(base_size = 18) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(2, "lines")) # give room for cutting between facets


##### Note: For best results, print on A4 colored cardstock. You can do this
##### using drawer 5 on the M&S hallway printer (but we had to do it by 
##### printing first, then copying).
