install.packages("dplyr")
install.packages("audio")

library("dplyr")
library("audio")
notes <- c(Gm = 20, A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)
pitch <- "C C B C C B C D D F F E C D B Gm E C D Gm C F E E D D C B C C B C C B C D D F F E C D B Gm E C D G C F E E D D"
duration <- c(3, 0.5, 0.5, 3, 0.5, 0.5, 1.5, 0.5, 1.5, 0.5, 3, 0.5, 0.5, 1.5, 0.5, 1, 0.5, 0.5, 3, 0.5, 0.5, 1.5, 0.5, 1.5, 0.5, 3, 0.5, 0.5, 3, 0.5, 0.5, 3, 0.5, 0.5, 1.5, 0.5, 1.5, 0.5, 3, 0.5, 0.5, 1.5, 0.5, 1, 0.5, 0.5, 3, 0.5, 0.5, 1.5, 0.5, 1.5, 0.5, 3)
JP <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                 duration = duration)

JP <-
  JP %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
  {suppressWarnings(as.numeric(.))} %>%
    ifelse(is.na(.), 4, .),
  note = notes[substr(pitch, 1, 1)],
  note = note + grepl("#", pitch) -
    grepl("b", pitch) + octave * 12 +
    12 * (note < 8),
  freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 85
sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

JP_jam <-
  mapply(make_sine, JP$freq, JP$duration) %>%
  do.call("c", .)

play(JP_jam)
