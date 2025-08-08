;; Sombrero plot: sin(x)x
X = - 8 * 0.2 mesh-x 80 80
Y = - 8 * 0.2 mesh-y 80 80
R = sqrt + square X square Y
Z = % R sin R 
surf stack X stack Y Z
format-plot "set hidden3d; set pm3d; set xlabel 'x'; set ylabel 'y'; set zlabel 'z';"
