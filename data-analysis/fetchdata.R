if (!require("pacman")) install.packages("pacman")
pacman::p_load(googlesheets, dplyr)

library(googlesheets)
suppressMessages(library(dplyr))

misaligned_answers_sheet <- gs_key("1KwhoGLuazPAeUcsVb_a3LwPCjXHQmuoTSn-KJwOXOXI")

gs_ws_ls(misaligned_answers_sheet)
df <- gs_read(misaligned_answers_sheet, ws = "Risposte del modulo 1")





  

