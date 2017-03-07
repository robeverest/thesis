# vim: filetype=gnuplot tw=0 nospell

set title "SMVM (Queen\\_4147)"

set terminal pdf size 3,2.2
set output "smvm.pdf"

set key on
set key bottom right

set xlabel "# Threads"
# set logscale x
# set xrange [1:25]

# set xtics (4, 8, 12, 16, 20, 24)
# set xtics (2, 4, 6, 8, 10, 12, 14, 16, 18, 20)

set colorsequence classic
set ylabel "Speedup vs. MKL \\@ 1 Thread"
# set logscale y
# set yrange [0.4:150]

# set key invert

plot    'smvm.dat' using 1:2 title "Intel MKL"               ls 9 lw 4 with linespoints, \
        'smvm.dat' using 1:3 title "Accelerate"              ls 3 lw 4 with linespoints, \
        'smvm.dat' using 1:4 title "Accelerate + Sequences"  ls 2 lw 4 with linespoints

