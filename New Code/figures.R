### Load data #######################################################

Scenarios <- read.csv("Output/Scenarios.csv")

### Example plot ####################################################

# Select country
tmpplot <- Scenarios %>% filter(Country=="Germany")

# Select reasonable IFRs
tmpplot <- tmpplot %>% filter(IFRs%in%c("Levin","Levin_m25","Levin_p25",
                                        "Verity","Verity_Germany",
                                        "Salje","Salje_Germany"))

# Select reasonable case scenarios
tmpplot <- tmpplot %>% filter(Cases%in%c("Levin2","Deaths","Cases","NewCases"))

# Plot
pdf(file="Output/Figure_Germany.pdf")
dotchart(tmpplot$Result[tmpplot$Cases=="Levin2"][7:1],xlim=c(0,0.038),pch=16,col="red",
         labels=c("Levin et al.","Verity et al.","Salje et al.",
                  "Levin et al. +25%","Levin et al. -25%",
                  "Verity et al. scaled","Salje et al. scaled")[7:1],
         main="Germany",
         panel.first={rect(xleft=0.001,xright=0.005,ybottom=-3,ytop=15,
                           col=rgb(0,0,1,alpha=0.15),border=NA);grid()})
points(y=7:1,x=tmpplot$Result[tmpplot$Cases=="Deaths"][7:1],pch=16,col="blue")  
points(y=7:1,x=tmpplot$Result[tmpplot$Cases=="Cases"][7:1],pch=16,col="purple")  
points(y=7:1,x=tmpplot$Result[tmpplot$Cases=="NewCases"][7:1],pch=16,col="orange")  
legend(x=0.025,y=7.4,pch=16,col=c("red","blue","purple","orange"),
       legend=c("Rectangular","Deaths/indirect","Cumulative cases","New cases"),title="Age structure",bg="white")  
abline(h=4.5)
abline(h=2.5)
dev.off()


### Another plot ####################################################

# Select reasonable IFRs
tmpplot <- Scenarios 
tmpplot$IC <- unlist(lapply(strsplit(tmpplot$IFRs,"_"),function(x) {y <- x[-1];
if(length(y)==0) y <- "Gen";return(y)}))

tmpplot <- tmpplot %>% filter(IC==Country|IC%in%c("Gen","p25","m25"))

# Select reasonable case scenarios
tmpplot <- tmpplot %>% filter(Cases%in%c("Levin2","Deaths","Cases"))

# Select reasonable IFRs
tmpplot <- tmpplot %>% filter(IFRs%in%c("Levin","Levin_m25","Levin_p25",
                                        "Verity","Salje",
                                        levels(interaction(c("Verity","Salje"),unique(Scenarios$Country),sep="_"))))

tmp <- tmpplot %>% group_by(Country) %>% summarise(m=median(Result),
                                                   lo=quantile(Result,0.1),
                                                   hi=quantile(Result,0.9),
                                                   min=min(Result),
                                                   max=max(Result))

pdf(file="Output/Figure_Overview.pdf")
dotchart(labels=tmp$Country,x=tmp$m,xlab="IFR",xlim=c(0,0.10),pch=16,
         panel.first={rect(xleft=0.001,xright=0.005,ybottom=-3,ytop=15,
                           col=rgb(0,0,1,alpha=0.15),border=NA);grid()},
         main="Country overview")
for(i in 1:length(unique(Scenarios$Country))) {
  lines(y=c(i,i),x=c(tmp$lo[i],tmp$hi[i]))
  lines(y=c(i,i),x=c(tmp$min[i],tmp$max[i]),lty=2)
}

legend(x=0.08,y=2.5,pch=c(16,-1,-1),lty=c(0,1,2),legend=c("Median","80%","Min/Max"),bg="white")
dev.off()
