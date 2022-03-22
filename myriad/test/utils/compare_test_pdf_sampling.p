set terminal png size 1600,1200
set output 'test_pdf_sampled_function.png'

set title "Comparison between a PDF ({/:Italic Probability Density Function}) and its actual sampling"
set xlabel "Sample values"
set ylabel "Sample Probabilities / Frequencies"
plot "test_pdf_sampled_function.dat" using 1:2 with lines title "User-specified PDF", "test_pdf_actual_samples.dat" using 1:2 with lines title "Actual sampling (with the alias method)",
