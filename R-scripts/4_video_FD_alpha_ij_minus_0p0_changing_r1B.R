
library(ggplot2)
library(dplyr)
library(av)
library(latex2exp)
library(data.table)
####################################################################
# Create grid for r1 and r2, X and Y

A11 = -1
A12 = 0
A21 = 0
A22 = -1

# Load data

D_aux2  <-  1/100

r2B_aux = round(0.05,3)

data_D_aux2 <- readr::read_csv("Results/FD_2sp_toy_DOM_PERFECT_V5_100xD.csv") %>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0,
         r1B=round(r1B,3)) %>%
  filter(round(r2B,3) == r2B_aux, D == D_aux2)

r1B <- data_D_aux2$r1B %>% unique() %>% sort()

data_D_aux2$type <- "Not feasible"
data_D_aux2$type[data_D_aux2$Feasible == TRUE] <- "Feasible"
data_D_aux2$type[data_D_aux2$Feasible_stable == TRUE] <- "Feasible and Stable"

r1B <- data_D_aux2$r1B %>% unique() %>% sort()

# Definir la ruta y nombre del archivo de video
output_video <- "Figures/Videos/alpha_ij_minus_0p0_D_0p01.mp4"

frame_count <- 0

av::av_capture_graphics({
  for (i in 1:length(r1B)) {
    r1B_aux <- round(r1B[i],3)
    filtered_data <- data_D_aux2[data_D_aux2$r1B == r1B_aux, ]
    
    # Crear gráfico con ggplot
    p <- ggplot(filtered_data, aes(x = r1A, y = r2A, fill = as.factor(type))) +
      geom_tile() +
      labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux2,
                              ",\\alpha_{11}=", A11,
                              ",\\alpha_{22}=", A22,
                              ",\\alpha_{12}=", A12,
                              ",\\alpha_{21}=", A21,
                              ",r_{1B}=", r1B_aux,
                              ",r_{2B}=", r2B_aux,
                              "$")),
           x = TeX(paste0("$r_{1A}$")),
           y = TeX(paste0("$r_{2A}$"))) +
      scale_fill_manual(values = c("Feasible and Stable" = "#3B9AB2", 
                                   "Feasible" = "#E4D00A",            
                                   "Not feasible" = "#F21A00"),
                        name = "") +
      coord_fixed() +theme_bw()
    
    print(p)
    #Sys.sleep(1)
    
    frame_count <- frame_count + 1  # Incrementar el contador de frames
    cat("Frame", frame_count, "added\n")  # Mostrar el número de frame añadido
  }
}, width = 640, height = 480, framerate = 4, output = output_video)


#####################################################
#####################################################

D_aux3  <-  1/10

data_D_aux3 <- readr::read_csv("Results/FD_2sp_toy_DOM_PERFECT_V5_10xD.csv") %>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0,
         r1B=round(r1B,3)) %>%
  filter(round(r2B,3) == r2B_aux, D == D_aux3)

data_D_aux3$type <- "Not feasible"
data_D_aux3$type[data_D_aux3$Feasible == TRUE] <- "Feasible"
data_D_aux3$type[data_D_aux3$Feasible_stable == TRUE] <- "Feasible and Stable"

r1B <- data_D_aux3$r1B %>% unique() %>% sort()

# Definir la ruta y nombre del archivo de video
output_video <- "Figures/Videos/alpha_ij_minus_0p0_D_0p1.mp4"

frame_count <- 0

av::av_capture_graphics({
  for (i in 1:length(r1B)) {
    r1B_aux <- round(r1B[i],3)
    filtered_data <- data_D_aux3[data_D_aux3$r1B == r1B_aux, ]
    
    # Crear gráfico con ggplot
    p <- ggplot(filtered_data, aes(x = r1A, y = r2A, fill = as.factor(type))) +
      geom_tile() +
      labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux3,
                              ",\\alpha_{11}=", A11,
                              ",\\alpha_{22}=", A22,
                              ",\\alpha_{12}=", A12,
                              ",\\alpha_{21}=", A21,
                              ",r_{1B}=", r1B_aux,
                              ",r_{2B}=", r2B_aux,
                              "$")),
           x = TeX(paste0("$r_{1A}$")),
           y = TeX(paste0("$r_{2A}$"))) +
      scale_fill_manual(values = c("Feasible and Stable" = "#3B9AB2", 
                                   "Feasible" = "#E4D00A",            
                                   "Not feasible" = "#F21A00"),
                        name = "") +
      coord_fixed() +theme_bw()
    
    print(p)
    #Sys.sleep(1)
    
    frame_count <- frame_count + 1  # Incrementar el contador de frames
    cat("Frame", frame_count, "added\n")  # Mostrar el número de frame añadido
  }
}, width = 640, height = 480, framerate = 4, output = output_video)

#####################################################
#####################################################

D_aux4  <-  1/1

data_D_aux4 <- readr::read_csv("Results/FD_2sp_toy_DOM_PERFECT_V5_1xD.csv") %>% 
  mutate(Feasible = number_feasible_sol>0,
         Feasible_stable = number_feasible_stable_sol>0,
         r1B=round(r1B,3)) %>%
  filter(round(r2B,3) == r2B_aux, D == D_aux4)

data_D_aux4$type <- "Not feasible"
data_D_aux4$type[data_D_aux4$Feasible == TRUE] <- "Feasible"
data_D_aux4$type[data_D_aux4$Feasible_stable == TRUE] <- "Feasible and Stable"

r1B <- data_D_aux4$r1B %>% unique() %>% sort()

# Definir la ruta y nombre del archivo de video
output_video <- "Figures/Videos/alpha_ij_minus_0p0_D_1p0.mp4"

frame_count <- 0

av::av_capture_graphics({
  for (i in 1:length(r1B)) {
    r1B_aux <- round(r1B[i],3)
    filtered_data <- data_D_aux4[data_D_aux4$r1B == r1B_aux, ]
    
    # Crear gráfico con ggplot
    p <- ggplot(filtered_data, aes(x = r1A, y = r2A, fill = as.factor(type))) +
      geom_tile() +
      labs(title = TeX(paste0("$D_{1}=D_{2}=", D_aux4,
                              ",\\alpha_{11}=", A11,
                              ",\\alpha_{22}=", A22,
                              ",\\alpha_{12}=", A12,
                              ",\\alpha_{21}=", A21,
                              ",r_{1B}=", r1B_aux,
                              ",r_{2B}=", r2B_aux,
                              "$")),
           x = TeX(paste0("$r_{1A}$")),
           y = TeX(paste0("$r_{2A}$"))) +
      scale_fill_manual(values = c("Feasible and Stable" = "#3B9AB2", 
                                   "Feasible" = "#E4D00A",            
                                   "Not feasible" = "#F21A00"),
                        name = "") +
      coord_fixed() +theme_bw()
    
    print(p)
    #Sys.sleep(1)
    
    frame_count <- frame_count + 1  # Incrementar el contador de frames
    cat("Frame", frame_count, "added\n")  # Mostrar el número de frame añadido
  }
}, width = 640, height = 480, framerate = 4, output = output_video)
