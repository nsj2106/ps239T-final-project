

## Short Description

I am studying what predicts women’s participation in politics in the global south. I acquired data through my adviser from large-scale surveys undertaken by a think tank in Delhi in 2014. Several questions in the survey ask men and women about their political participation aside from voting, i.e. did you attend rallies, distribute leaflets, participate in election meetings, canvass door-to-door. Previous scholarship has suggested that women participate in such activities less because of the caste system, and that the women who actually engage in such campaigning are from a special group called the Schedule Tribe. Why? Because they are supposedly less bound by caste patriarchy. To test these assumptions, I use my R skills to subset, analyze (using cross-tabs etc.), and plot data to see whether this is the case. After loading ggplot, reshape, and other packages (to ensure my regressions provide robust standard errors), I first subset my data by gender. I then start ordering/sorting my data by two variables – a) state and b) whether a woman participated in political campaigning. I use both basic R plotting and ggplot2 – I wanted to master both and ensure that ggplotting was in fact easier. What took a while was figuring out why at first ggplot2 wasn’t working, and it turns out I needed to apply a “melt” function to a subset of my data using the “reshape” package. Anyway, the plots show that women participate more in such activities compared to other regions. They have an unusually higher level of literacy than other regions and are for the most part more interested in politics. I finally run two regressions. The first regression creates an index of all four variables of political campaigning (I am able to do this because of the high chrombach’s alpha). I use ‘lm’ and also make sure to run an interaction between the Northeast dummy AND being a member of the Scheduled Tribe. Before running my regressions I had to separate each and every variable of interest, as well as combine some (such as removing “somewhat interested” and simply making it “interested”). 

## Dependencies

1. R, version 3.1

## Files

### Data
Unfortunatley, my adviser has not allowed me to share this dataset with anyone else. I am, however, including a knitted html file of the code,
plots and regressions. 

### Code
1. Northeast Women10_final.R (raw code)
2. Northeast Women10_Analysis.html: code as well as analyses and visualizations.

### Results
1. Rplot1.jpeg: Graphs men and women campaigners by region. 
2. Rplot2.jpeg: Gender gap between men and women in political participation, without Arunachal Pradesh. 
3. Rplot3.jpeg: Graphs men and women in associations by region. 
4. Rplot4.jpeg: Graphs men and women literacy by region. 
5. Rplot5.jpeg: Graphs "level of independence" of women. 
6. Rplot6.jpeg: Graphs men and women "interest in politics".
7. regression1.txt: Summarizes the results of OLS regression, with women's level of political campaigning as DV.
8. regression2.txt: Summarizes the results of OLS regression, with Northeast women's level of political campaigning as DV.

