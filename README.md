
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Academic bias

I sometimes still find people claiming there is no political bias in
academia. So I made these graphs to help explain the problem.

The density plot below shows the distribution of US counties by
Democratic vote share in 2020. The coloured lines show the proportion of
registered Democrats, out of registered Republicans and Democrats, in
five academic disciplines. I’ve added a zoom inset, in case you strain
your eyes trying to spot the number of Republicans among history
faculty.

![](README_files/figure-gfm/plot1-1.png)<!-- -->

To get more disciplines, we can use Langbert (2018), who looks at
professors in liberal arts colleges. I haven’t added the proportion of
the population in more Democratic counties, since it’s essentially zero
for most of this data. But well done to engineering: a full 26% of the
US population lives in counties which are more Democratic than
engineering faculty.

![](README_files/figure-gfm/plot2-1.png)<!-- -->

# FAQ

- Is this satire?

  A little bit, yes. But the data is real.

- This isn’t fair, you’re comparing registered Democrats in faculty to
  Democrat voters in the US.

  Short answer: dude, suppose I’m out by a factor of ten.

  Long answer: in the 2016 data, we can correct for this by assuming
  that unregistered faculty split their vote equally between Democrat
  and Republican. On this ridiculously generous assumption, our data
  looks like this:

![](README_files/figure-gfm/plot3-1.png)<!-- -->
