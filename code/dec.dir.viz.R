library(tidyverse)
library(janitor)
library(ggtext)

#### Data Set ####
#  http://scdb.wustl.edu/data.php - website for decision data (use justice centered data)
ideo <- read_csv("SCDB_2022_01_justiceCentered_Citation.csv")

ideo.r <- ideo %>% filter(chief=="Roberts") %>%
  mutate(decisionDirection=case_when(
    decisionDirection==1 ~ "con",
    decisionDirection==2 ~ "lib",
    decisionDirection==3 ~ "unk"
  ))

#### direction by term & cohort ####

## decision, tenure and mqscore data

#  https://mqscores.lsa.umich.edu/measures.php - website to dowonload the mq-scores

mqscore <- read.csv("justices.csv")

# SCOTUS Justice tenure (scraped from https://ballotpedia.org/The_Roberts_Court)
tenure <- readxl::read_xlsx("RC_Justice_Tenure.xlsx")

# create data set with tenures and mean mqscores for each Justice
tenure.mq <- mqscore %>% 
  filter(term>=2005) %>% 
  group_by(justiceName) %>% 
  summarize(Mean=mean(post_mn),N=n(),term=min(term)) %>% 
  mutate(justiceName=str_extract(justiceName,"[A-Z][a-z]+")) %>%
  mutate(justiceName=replace(justiceName,justiceName=="Connor","O'Connor")) %>%
  arrange(term)
tenure.mq <- tenure.mq %>% 
  add_row(tenure.mq[c(3,5),]) %>%
  mutate(order=c(4,3,7,1,8,2,5,0,0,6,1,2,3,4,5,7,8),
         initial=c(1,1,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0))

# mean mq.score for eah cohort
avg.mq <- mqscore %>%
  filter(term>=2005) %>%
  mutate(term2 = case_when(
    term==2005 ~ 2005.5,
    term >= 2006 & term < 2009 ~ 2007.5,
    term == 2009 ~ 2009.5,
    term > 2009 & term < 2016 ~ 2013,
    term == 2016 ~ 2016.5,
    term == 2017 ~ 2017.5,
    term >= 2018 & term < 2020 ~ 2019,
    term >= 2020 ~ 2021
  )) %>% group_by(term2) %>%
  summarize(avg.post=round(mean(post_mn),2),med.post=median(post_mn),n=n()) %>%
  mutate(ideo=ifelse(avg.post<0,"L","C"))

#  mean mq-score for each term
avg.mq.term <- mqscore %>%
  filter(term >= 2005) %>%
  group_by(term) %>%
  summarize(avg.post=round(mean(post_mn),2),med.post=median(post_mn),n=n()) %>%
  mutate(ideo=ifelse(avg.post<0,"L","C"),term=term+.5)

#  calculate decision direction proportions and format for labeling
decision.label <- ideo.r %>% 
  mutate(term2 = case_when(
    term==2005 ~ 2005.5,
    term >= 2006 & term < 2009 ~ 2007.5,
    term == 2009 ~ 2009.5,
    term > 2009 & term < 2016 ~ 2013,
    term == 2016 ~ 2016.5,
    term == 2017 ~ 2017.5,
    term >= 2018 & term < 2020 ~ 2019,
    term >= 2020 ~ 2021
  )) %>%
  mutate(term=term2) %>%
  select(decisionDirection,term,caseId) %>%
  distinct() %>%
  drop_na() %>%
  tabyl(term,decisionDirection) %>%
  adorn_totals(c("row","col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits=1) %>%
  adorn_ns() %>% 
  data.frame() %>% 
  `colnames<-`(c("vote","con","lib","unk","total")) %>% 
  separate_wider_delim(con," (",names=c("con","con_num")) %>% 
  mutate(con=str_trim(con),con2=as.numeric(str_replace(con,"%","")),con_num=str_replace(con_num,"\\)","")) %>% 
  separate_wider_delim(lib," (",names=c("lib","lib_num")) %>% 
  mutate(lib=str_trim(lib),lib2=as.numeric(str_replace(lib,"%","")),lib_num=str_replace(lib_num,"\\)","")) %>%   
  separate_wider_delim(unk," (",names=c("unk","unk_num")) %>% 
  mutate(unk=str_trim(unk),unk_num=str_replace(unk_num,"\\)","")) %>% 
  separate_wider_delim(total,"(",names=c(NA,"total")) %>%
  mutate(total=str_replace(total,"\\)","")) %>%
  mutate(vote=replace(vote,vote=="Total",2026.25)) %>%
  mutate(lab=case_when(
    con2>lib2 ~ con,
    con2<lib2 ~ lib
  ),
  ideo=case_when(
    con2>lib2 ~ "C",
    con2<lib2 ~ "L"
  )) %>%
  select(vote,lab,ideo)

# create decision % labels for each term
dec.dir.term <- ideo.r %>% 
  select(decisionDirection,term,caseId) %>%
  distinct() %>%
  drop_na() %>%
  tabyl(term,decisionDirection) %>%
  adorn_totals(c("row","col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits=1) %>%
  adorn_ns() %>% 
  data.frame() %>% 
  `colnames<-`(c("vote","con","lib","unk","total")) %>% 
  separate_wider_delim(con," (",names=c("con","con_num")) %>% 
  mutate(con=str_trim(con),con2=as.numeric(str_replace(con,"%","")),con_num=str_replace(con_num,"\\)","")) %>% 
  separate_wider_delim(lib," (",names=c("lib","lib_num")) %>% 
  mutate(lib=str_trim(lib),lib2=as.numeric(str_replace(lib,"%","")),lib_num=str_replace(lib_num,"\\)","")) %>%   
  separate_wider_delim(unk," (",names=c("unk","unk_num")) %>% 
  mutate(unk=str_trim(unk),unk_num=str_replace(unk_num,"\\)","")) %>% 
  separate_wider_delim(total,"(",names=c(NA,"total")) %>%
  mutate(total=str_replace(total,"\\)","")) %>%
  mutate(vote=replace(vote,vote=="Total",2025.5)) %>%
  mutate(lab=case_when(
    con2>lib2 ~ con,
    con2<lib2 ~ lib
  ),
  ideo=case_when(
    con2>lib2 ~ "C",
    con2<lib2 ~ "L"
  )) %>%
  select(vote,lab,ideo)


# labels for Justice changes
tenure.ch <- data.frame(
  change=c("O'Connor &#8594; Alito","Souter &#8594; Sotomayor","Stevens &#8594; Kagan",
           "Scalia &#8594; Gorsuch","Kennedy &#8594; Kavanaugh","Ginsburg &#8594; Barrett",
           "Breyer &#8594; Jackson"),
  x=c(2006,2009,2010,2016.5,2018,2020,2022),
  y=seq(.5,6.5,1)
)

# text for subtitles and caption
sub.text <- "The higher percentage of cases decided in either a <span style='color: blue'>**Liberal**</span> or <span style='color: red'>**Conservative**</span> direction, 
indicating which ideology was favored for each term/cohort.  The interesting term/cohort is **2009** when <span style='color: #7f7f7f'>**neither**</span> was favored.  (Not all decisions could be classified by ideology, 
so the higher percentage may be below 50%).<br>
Also included are the tenure and ideology of each SCOTUS Justice during the Roberts Court, their average 
Martin-Quinn (MQ) score during their tenure on the Roberts Court and the average MQ score for each 
term/cohort.<br>(Data for 2022 term not available as of 7/27/2023)."
cap.text <- "\nMartin-Quinn Scores data from https://mqscores.lsa.umich.edu/measures.php\nDecison Direction data from http://scdb.wustl.edu/data.php"


##### Generate Viz

tenure %>% 
  separate(Tenure,into=c("start","end"),sep="-") %>%
  mutate(end=as.numeric(replace(end,end=="Present","2023")),
         start=as.numeric(ifelse(as.numeric(start)<2005,"2005",start))) %>%
  arrange(desc(end),desc(start)) %>%
  mutate(repl=c(6,5,4,3,2,1,0,7,8,6,5,4,3,2,1,0),
         Justice=fct_reorder(Justice,repl),
         ideo=c("L","C","C","C","L","L","C","C","C","L","L","C","C","L","L","C"),
         end2=ifelse(end==2023,NA,end)) %>%
  
  ggplot()+
  geom_segment(aes(x=start+.2,y=repl,xend=end-.2,yend=repl,col=ideo),linewidth=3,lineend="round")+
  geom_vline(aes(xintercept=c(rep(NA,9),2022,2020,2018,2016,2010,2009,2006)
  ),col="gray30",lty=2)+
  geom_vline(xintercept=2017,col="gray30",lty=2)+
  geom_segment(data=data.frame(
    x=c(2007,2008,2011:2015,2019,2021),
    xend=c(2007,2008,2011:2015,2019,2021),
    y=rep(8.475,9),
    yend=rep(10.65,9)),
    aes(x=x,xend=xend,y=y,yend=yend),lty=2,col="gray")+

  geom_text(data=tenure.mq %>% filter(initial==1),aes(x=rep(2003,9),y=order,label=round(Mean,2)),size=4.5)+
  geom_text(data=tenure.mq %>% filter(initial==1),aes(x=rep(2001,9),y=order,label=justiceName),size=4.5)+
  geom_text(data=tenure.mq %>% filter(initial==0),aes(x=rep(2025,8),y=order,label=round(Mean,2)),size=4.5)+
  geom_text(data=tenure.mq %>% filter(initial==0) %>% add_row(justiceName="Jackson",order=6),
            aes(x=rep(2027,9),y=order,label=justiceName),size=4.5)+
  
  ggtext::geom_richtext(data=data.frame(x=c(2003.2,2024.8),
                                        y=c(rep(9,2)),
                                        label=c("*MQ Score*","*MQ Score*")),
                        aes(x=x,y=y,label=label),label.color=NA,fill="#F1F1F2",size=5)+
  ggtext::geom_richtext(data=data.frame(x=c(2003.2,2024.8),
                                        y=c(rep(-1,2)),
                                        label=c("*MQ Score*","*MQ Score*")),
                        aes(x=x,y=y,label=label),label.color=NA,fill="#F1F1F2",size=5)+
  ggtext::geom_richtext(data=data.frame(x=c(1999.5,2028.5),y=c(5,5),
                                        label=c("***2005 Court***","***Current Court***"),
                                        angle=c(90,270)),
                        aes(x=x,y=y,label=label,angle=angle),label.color=NA,fill="#F1F1F2",size=5)+
  
  geom_segment(aes(x=2002,xend=2026,y=8.5,yend=8.5))+
  geom_segment(aes(x=2002,xend=2026,y=-.5,yend=-.5))+
  geom_segment(aes(x=2000,xend=2028,y=9.5,yend=9.5),size=2)+
  geom_segment(aes(x=2000,xend=2028,y=-1.5,yend=-1.5),size=2)+
  
  geom_text(data=dec.dir.term,aes(x=as.numeric(vote)+.5,y=rep(10,18),label=lab,col=ideo),size=4.755)+
  geom_text(data=decision.label,aes(x=as.numeric(vote),y=rep(-2,9),label=lab,col=ideo),size=4.5)+
  geom_text(data=avg.mq.term,aes(x=term,y=rep(9,17),label=avg.post,col=ideo),size=4.5)+
  geom_text(data=avg.mq,aes(x=term2,y=rep(-1,8),label=avg.post,col=ideo),size=4.5)+
  geom_text(aes(x=2009.5,y=-2,label="49.5%"),size=5,col="#7f7f7f")+
  geom_text(aes(x=2009.5,y=10,label="49.5%"),size=5,col="#7f7f7f")+
  geom_text(aes(x=2022.5,y=10,label="???"),size=5,col="#7f7f7f")+
  
  
  ggtext::geom_richtext(aes(x=2002.5,y=10,label="***Decision Direction*** &#8594;<br>*by term*"),label.color=NA,fill="#F1F1F2",size=5.5)+
  ggtext::geom_richtext(aes(x=2002.5,y=-2,label="***Decision Direction*** &#8594;<br>*by cohort*"),label.color=NA,fill="#F1F1F2",size=5.5)+
  
  ggtext::geom_richtext(aes(x=2024.5,y=10,label="***Overall:***"),label.color=NA,fill="#F1F1F2",size=5.5)+
  ggtext::geom_richtext(aes(x=2024.5,y=-2,label="***Overall:***"),label.color=NA,fill="#F1F1F2",size=5.5)+
  
  ggtext::geom_richtext(data=tenure.ch,aes(x=x,y=y,label=change),fill="gray80")+
  
  labs(title="Roberts Court:\nIdeological Direction of Decisions by SCOTUS Term & Cohort",
       subtitle=sub.text,
       caption=cap.text
  ) +
  scale_x_continuous(breaks=seq(2005,2023,1),labels=c(seq(2005,2023,1)),
                     limits=c(1999.5,2028.5),sec.axis=dup_axis())+
  scale_color_manual(values=c("red","blue"))+
  theme_minimal()+
  theme(
    panel.grid=element_blank(),
    plot.title=element_text(size=24,hjust=.5),
    plot.subtitle=element_textbox_simple(
      width=grid::unit(.75,"npc"),
      size=14,
      lineheight=1,
      padding=margin(5.5,5.5,5.5,5.5),
      margin=margin(0,0,5.5,0)),
    plot.caption = element_text(hjust=.5,size=10),
    axis.title=element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_text(size=12),
    legend.position = "none",
    panel.background =element_rect(fill="#F1F1F2",color="#F1F1F2"),
    plot.background=element_rect(fill="#F1F1F2",color="#F1F1F2"),
    plot.margin=margin(.5,.5,.5,.5,"cm")
  ) +
  coord_cartesian(ylim=c(-2,10),clip="off")

ggsave("dec.dir.cohort2.png",dpi=320,height=12,width=20)

###########################

#### by term and issue ####

# data prep
totals <- ideo.r %>% 
  select(caseId,term,issueArea) %>% 
  distinct() %>% 
  select(-caseId) %>%
  group_by(term,issueArea)%>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from=term,values_from=n) %>%
  arrange(issueArea) %>%
  drop_na(issueArea) %>%
  select(-issueArea)

row_tots=data.frame(tots=rowSums(totals,na.rm=T),issue=issueArea_label) %>% 
  arrange(tots) %>%
  mutate(issue=fct_reorder(as.factor(issue),tots))

xxx.iba=data.frame()
for (i in 2005:2021) {
  print(i)
  id.x <- 
    ideo.r %>%
    drop_na(decisionDirection) %>%
    drop_na(issueArea) %>%
    select(caseId,issueArea,decisionDirection,term) %>%
    distinct() %>%
    tabyl(issueArea,decisionDirection,term) %>%
    #adorn_totals(c("col","row")) %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits=1) %>%
    adorn_ns() %>% .[[as.character(i)]] %>%
    mutate(term=i) %>%
    separate_wider_delim(con," (",names=c("con",NA)) %>% 
    mutate(con=str_trim(con)) %>% 
    separate_wider_delim(lib," (",names=c("lib",NA)) %>% 
    mutate(lib=str_trim(lib)) %>% 
    mutate(con2=as.numeric(str_replace(con,"%","")),
           lib2=as.numeric(str_replace(lib,"%","")),
           dir=case_when(
             con2>lib2 ~ "C",
             con2<lib2 ~ "L",
             con2==lib2 ~ "N"
           ),
           dir.lab=case_when(
             dir=="C"~con,
             dir=="L"~lib,
             dir=="N"~con
           )
    ) %>%
    select(issueArea,term,dir,dir.lab)
  xxx.iba <- rbind(xxx.iba,id.x)
}
xxx.iba <- xxx.iba %>% 
  arrange(issueArea,term) %>%
  mutate(id=seq(1:nrow(.)))
xxx.iba.overall <- ideo.r %>%
  drop_na(decisionDirection) %>%
  drop_na(issueArea) %>%
  select(caseId,issueArea,decisionDirection) %>%
  distinct() %>%
  tabyl(issueArea,decisionDirection) %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits=1) %>%
  adorn_ns() %>% data.frame() %>%
  mutate(issueArea_label = c("Criminal\nProcedure","Civil Rights","First\nAmendment",
                             "Due Process","Privacy","Attorneys","Unions","Economic\nActivity",
                             "Judicial\nPower","Federalism","Interstate\nRelations",
                             "Federal\nTaxation","Miscellaneous","Private\nAction")) %>%
  relocate(issueArea_label,.after=issueArea) %>%
  separate_wider_delim(con," (",names=c("con","con_num")) %>% 
  mutate(con=str_trim(con),con_num=str_replace(con_num,"\\)","")) %>% 
  separate_wider_delim(lib," (",names=c("lib","lib_num")) %>% 
  mutate(lib=str_trim(lib),lib_num=str_replace(lib_num,"\\)","")) %>%   
  separate_wider_delim(unk," (",names=c("unk","unk_num")) %>% 
  mutate(unk=str_trim(unk),unk_num=str_replace(unk_num,"\\)","")) %>% 
  separate_wider_delim(Total,"(",names=c(NA,"total")) %>%
  mutate(total=as.numeric(str_replace(total,"\\)",""))) %>% 
  arrange(desc(total)) %>%
  mutate(id=14:1)

sub.text <- "The higher percentage of cases decided in either a 
<span style='color: blue'>**Liberal**</span> or <span style='color: red'>**Conservative**</span> direction, 
(or <span style='color: black'>**neither**</span>),indicating which ideology 
was favored for each term for the particular issue area.  The total number of cases for each issuea area
as well as the overall percentages for each ideology are also included.<br>
The shading of each box indicates the proportion of term's cases for the issue area.  Unshaded boxes (white) 
indicate no cases for the issue area were heard during the term.  The values of 0.0% indicate no cases were
classified in either ideological direction.<br>
(Data for 2022 term not available as of 7/27/2023)."
cap.text <- "\nData from The Supreme Court Database: http://scdb.wustl.edu/data.php"

## Generate Viz

ideo.r %>% group_by(term,issueArea) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from=term,values_from=n) %>%
  arrange(issueArea) %>%
  drop_na(issueArea) %>%
  mutate(tots=rowSums(.,na.rm=T)-issueArea) %>% 
  mutate(issueArea=fct_reorder(as.factor(issueArea),tots)) %>%
  cbind(t(apply(totals,1,function(x) round(100*x/colSums(totals,na.rm=T),1))) %>% 
          data.frame()) %>% 
  select(issueArea,contains('X')) %>%
  pivot_longer(!issueArea,names_to="term",values_to="n") %>%
  mutate(term=str_replace(term,'X',""),n2=format(n,nsmall=1),id=seq(1:nrow(.))) %>%
  left_join(.,xxx.iba %>% select(id,dir,dir.lab)) %>%
  
  ggplot()+
  geom_tile(aes(as.factor(term),as.factor(issueArea),
                fill=as.numeric(n)),col="white")+
  scale_y_discrete(labels=issueArea_label)+
  scale_x_discrete(position="top")+
  scale_fill_gradient(low="gray50",high="gray80",na.value="white",
                      guide=guide_colorbar(title="Percentage of Cases/Term",
                                           title.vjust=.75,barheight=1.25,barwidth=10,vjust=3,
                                           direction="horizontal"))+

  geom_text(data=. %>% filter(dir=="C"),aes(x=as.factor(term),y=as.factor(issueArea),
                                            label=dir.lab),col="red",size=4.5)+
  geom_text(data=. %>% filter(dir=="L"),aes(x=as.factor(term),y=as.factor(issueArea),
                                            label=dir.lab),col="blue",size=4.5)+
  geom_text(data=. %>% filter(dir=="N"),aes(x=as.factor(term),y=as.factor(issueArea),
                                            label=dir.lab),col="black",size=4.5)+
  geom_text(data=data.frame(x=rep(18,14),y=1:14,label=row_tots$tots),
            aes(x=x,y=y,label=label),size=4.5)+
  geom_text(data=data.frame(x=1:17,y=rep(0,17),label=colSums(totals,na.rm=T)),
            aes(x=x,y=y,label=label),size=4.5)+
  geom_text(data=data.frame(x=1:18,y=rep(15,18),label=c(2005:2021,"Total\nCases")),
            aes(x=x,y=y,label=label),fontface="bold",size=4.5)+
  geom_text(data=data.frame(x=-0.5,y=0:14,label=c("Term Total",levels(row_tots$issue))),
            aes(x=x,y=y,label=label),fontface="bold",size=4.5)+
  
  geom_text(data=xxx.iba.overall,aes(x=rep(19,14),y=id,label=con),col="red",size=4.5)+
  geom_text(data=xxx.iba.overall,aes(x=rep(20,14),y=id,label=lib),col="blue",size=4.5)+
  geom_text(aes(x=19.5,y=15,label="Overall"),size=4.5,fontface="bold")+
  geom_segment(aes(x=18.6,xend=20.4,y=14.5,yend=14.5))+
  
  labs(
    title="Roberts Court Ideological Direction and Case Percentage\nby Term and Issue Area",
    subtitle=sub.text,
    caption=cap.text
  )+
  expand_limits(x=c(-1.75,21),y=c(-1,15.75))+
  theme_bw()+
  theme(legend.position=c(.5,1),
        legend.title=element_text(size=12),
        legend.background = element_rect(fill="#f1f1f1"),
        plot.title=element_text(hjust=.5,size=28),
        plot.subtitle=element_textbox_simple(
          width=grid::unit(.75,"npc"),
          size=14,
          lineheight=1,
          padding=margin(5.5,5.5,5.5,5.5),
          margin=margin(0,0,5.5,0)),
        plot.caption = element_text(hjust=.5,vjust=15,size=12),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill="#f1f1f1"),
        plot.background =element_rect(fill="#f1f1f1"))

ggsave("cases_by_issue_b.png",dpi=320,width=21,height=14)


###########################

#### by justice and term ####
# data prep
j_by_t <- data.frame()
for(i in 2005:2021) {
  j_by_t.temp <- full_join(
    (ideo.r %>%
       mutate(justiceName=str_extract(justiceName,"[A-Z][a-z]+")) %>%
       mutate(justiceName=replace(justiceName,justiceName=="Connor","O'Connor")) %>%
       drop_na(direction) %>%
       drop_na(decisionDirection) %>%
       filter(term==i) %>%
       tabyl(justiceName,direction,decisionDirection) %>%
       adorn_percentages("row") %>%
       adorn_pct_formatting(digits=1) %>%
       adorn_ns())$con %>% 
      data.frame(),
    (ideo.r %>%
       mutate(justiceName=str_extract(justiceName,"[A-Z][a-z]+")) %>%
       mutate(justiceName=replace(justiceName,justiceName=="Connor","O'Connor")) %>%
       drop_na(direction) %>%
       drop_na(decisionDirection) %>%
       filter(term==i) %>%
       tabyl(justiceName,direction,decisionDirection) %>%
       adorn_percentages("row") %>%
       adorn_pct_formatting(digits=1) %>%
       adorn_ns())$lib %>% 
      data.frame(),
    by=join_by(justiceName)
  ) %>%
    `colnames<-`(c("justice","con.c","lib.c","con.l","lib.l")) %>%
    
    separate_wider_delim(con.c," (",names=c("con.c",NA)) %>% 
    mutate(con.c=str_trim(con.c),con.c=as.numeric(str_replace(con.c,"%",""))) %>% 
    separate_wider_delim(lib.c," (",names=c("lib.c",NA)) %>% 
    mutate(lib.c=str_trim(lib.c),lib.c=as.numeric(str_replace(lib.c,"%",""))) %>%
    
    separate_wider_delim(con.l," (",names=c("con.l",NA)) %>% 
    mutate(con.l=str_trim(con.l),con.l=as.numeric(str_replace(con.l,"%",""))) %>% 
    separate_wider_delim(lib.l," (",names=c("lib.l",NA)) %>% 
    mutate(lib.l=str_trim(lib.l),lib.l=as.numeric(str_replace(lib.l,"%",""))) %>%
    mutate(term=i)
  j_by_t <- rbind(j_by_t,j_by_t.temp)
}
j_by_t <- j_by_t %>%
  select(justice,con.c,lib.l,term) %>%
  pivot_longer(cols=c("con.c","lib.l"),names_to="direction",values_to="percent") %>%
  mutate(justice=factor(justice,
                        levels=c("Roberts","Thomas","Alito","Gorsuch","Kavanaugh","Barrett",
                                 "Kennedy","Scalia","O'Connor",
                                 "Breyer","Sotomayor","Kagan","Ginsburg",
                                 "Stevens","Souter")))
## Generate Viz

j_by_t %>%
  ggplot()+
  geom_line(aes(term,percent,col=direction))+
  geom_point(aes(term,percent,col=direction))+
  xlim(2004,2022)+
  #ylim(0,101)+
  scale_color_manual(values=c("red","blue"),
                     name="Decision Direction",
                     labels=c("Conservative","Liberal"))+
  scale_y_continuous(limits=c(0,101),labels=scales::percent_format(scale=1))+
  labs(title="Ideology of Justice Votes by Decision Direction",
       subtitle="Percent of conservative votes for conservative decsions and liberal votes for liberal decisions",
       caption="\nData from The Supreme Court Database: http://scdb.wustl.edu/data.php"
  )+
  theme_minimal()+
  theme(legend.position="top",
        legend.key.size = unit(2,"cm"),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        plot.title=element_text(size=24,hjust=.5),
        plot.subtitle=element_text(size=16,hjust=.5),
        plot.caption=element_text(size=10,hjust=.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major=element_line(color="white"),
        axis.title=element_blank(),
        plot.background = element_rect(fill="#F1F1F1",color="white"),
        #panel.background = element_rect(color="white"),
        strip.background = element_rect(fill="gray70"),
        strip.text=element_text(size=12,face="bold"))+
  facet_wrap(.~justice,ncol=3)

ggsave("justice_dec_dir.png",dpi=320,width=12,height=12)
