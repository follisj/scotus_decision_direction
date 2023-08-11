library(tidyverse)
library(janitor)
library(gt)
library(gtExtras)
library(ggtext)

## data
#  http://scdb.wustl.edu/data.php - website for decision data (use justice centered data)
ideo <- read_csv("SCDB_2022_01_justiceCentered_Citation.csv")
ideo.r <- ideo %>% filter(chief=="Roberts") %>%
  mutate(decisionDirection=case_when(
    decisionDirection==1 ~ "con",
    decisionDirection==2 ~ "lib",
    decisionDirection==3 ~ "unk"
  ))

##### tables #####

#### by term ####
gt.by.term <- 
  ideo.r %>% 
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
  mutate(con=str_trim(con),con_num=str_replace(con_num,"\\)","")) %>% 
  separate_wider_delim(lib," (",names=c("lib","lib_num")) %>% 
  mutate(lib=str_trim(lib),lib_num=str_replace(lib_num,"\\)","")) %>%   
  separate_wider_delim(unk," (",names=c("unk","unk_num")) %>% 
  mutate(unk=str_trim(unk),unk_num=str_replace(unk_num,"\\)","")) %>% 
  separate_wider_delim(total,"(",names=c(NA,"total")) %>%
  mutate(total=str_replace(total,"\\)","")) %>%
  mutate(vote=replace(vote,vote=="Total","Overall")) %>%
  gt() %>%
  tab_spanner(
    label=md("**Decision Direction**"),
    columns=c(con,con_num,lib,lib_num,unk,unk_num)
  ) %>%
  gt_merge_stack(col1=con,col2=con_num,font_size = c("16px","15px"),
                 palette=c("red","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=lib,col2=lib_num,font_size = c("16px","14px"),
                 palette=c("blue","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=unk,col2=unk_num,font_size = c("16px","14px"),
                 palette=c("gray50","gray50"),font_weight=c("normal","normal")) %>%
  cols_label(
    vote=md("**Term**<br><br>"),
    con=html("<span style='color:red'>Conserative</span>"),
    lib=html("<span style='color:blue'>Liberal</span>"),
    unk=html("<span style='color:gray'>Neither</span>"),
    total=md("**Total<br>Cases**")
  ) %>%
  gt_highlight_rows(rows=c(3,9,10,12,14,15),fill="#cccccc") %>%
  gt_highlight_rows(rows=c(1,2,4,6,7,8,11,13,16,17,18),fill="#F1F1F2") %>%
  gt_highlight_rows(rows=5,fill="white") %>%
  cols_width(everything()~px(100)) %>%
  tab_header(
    title=md("Ideology of Decisions by Term"),
    subtitle=html("<svg width='15' height='12'><rect width='15' height='12' style='fill:#F1F1F1' stroke='black'/>",
                  "</svg><span style='color:red'> Conservative Direction&nbsp&nbsp</span>",
                  "<svg width='15' height='12'><rect width='15' height='12' style='fill:#cccccc' stroke='black'/>",
                  "</svg><span style='color:blue'> Liberal Direction&nbsp&nbsp</span>",
                  "<svg width='15' height='12'><rect width='15' height='12' style='fill:white' stroke='black'/>",
                  "</svg><span style='color:black'> Neither</span>")
  ) %>%
  tab_style(style=cell_borders(sides=c("top","bottom"),
                               color="gray30"),
            locations=cells_body(everything())
  ) %>%
  tab_options(
    column_labels.border.bottom.color = "gray30",
    column_labels.border.top.color="gray30",
    table_body.border.bottom.color = "gray30",
    table.border.top.color = "white",
    heading.border.bottom.color = "white",
    heading.align="left",
    column_labels.padding = px(6),
    heading.title.font.size=px(24),
    heading.subtitle.font.size = px(18)
  )

gtsave(gt.by.term,"gt.by.term.png")

#### by ruling ####
gt.by.ruling <- 
  ideo.r %>%
  mutate(decision=paste(majVotes,"-",minVotes)) %>% 
  mutate(decision=ifelse(majVotes+minVotes<8,"Other",decision)) %>%
  select(term,caseId,decisionDirection,decision) %>%
  distinct() %>%
  drop_na() %>%
  tabyl(decision,decisionDirection) %>%
  adorn_totals(c("row","col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits=1) %>%
  adorn_ns() %>%
  data.frame() %>% 
  `colnames<-`(c("vote","con","lib","unk","total")) %>% 
  separate_wider_delim(con," (",names=c("con","con_num")) %>% 
  mutate(con=str_trim(con),con_num=str_replace(con_num,"\\)","")) %>% 
  separate_wider_delim(lib," (",names=c("lib","lib_num")) %>% 
  mutate(lib=str_trim(lib),lib_num=str_replace(lib_num,"\\)","")) %>%   
  separate_wider_delim(unk," (",names=c("unk","unk_num")) %>% 
  mutate(unk=str_trim(unk),unk_num=str_replace(unk_num,"\\)","")) %>% 
  separate_wider_delim(total,"(",names=c(NA,"total")) %>%
  mutate(total=str_replace(total,"\\)","")) %>%
  mutate(vote2=ifelse(vote=="Total" | vote=="Other",NA,vote)) %>%
  separate_wider_delim(vote2,"- ",names=c("maj","dis")) %>%
  mutate(maj=as.numeric(maj),dis=as.numeric(dis),vot.tot=maj+dis) %>%
  arrange(desc(as.numeric(total))) %>%
  mutate(vote=replace(vote,vote=="Total","Overall")) %>%
  select(vote:total) %>%
  gt() %>%
  #tab_spanner(label="Decision",columns=vote) %>%
  tab_spanner(
    label=md("**Decision Direction**"),
    columns=c(con,con_num,lib,lib_num,unk,unk_num)
  ) %>%
  #tab_spanner(label="Total Cases",columns=total) %>%
  gt_merge_stack(col1=con,col2=con_num,font_size = c("16px","15px"),
                 palette=c("red","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=lib,col2=lib_num,font_size = c("16px","14px"),
                 palette=c("blue","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=unk,col2=unk_num,font_size = c("16px","14px"),
                 palette=c("gray50","gray50"),font_weight=c("normal","normal")) %>%
  cols_label(
    vote=md("**Ruling**<br><br>"),
    con=html("<span style='color:red'>Conserative</span>"),
    lib=html("<span style='color:blue'>Liberal</span>"),
    unk=html("<span style='color:gray'>Neither</span>"),
    total=md("**Total<br>Cases**")
  ) %>%
  gt_highlight_rows(rows=c(4,8,9,10),fill="gray80") %>%
  gt_highlight_rows(rows=c(1,2,3,5,6,11,12),fill="#F1F1F2") %>%
  gt_highlight_rows(rows=7,fill="white") %>%
  cols_width(everything()~px(100)) %>%
  tab_header(
    title=md("Ideology of Decisions by Ruling"),
    subtitle=html(html("<svg width='15' height='12'><rect width='15' height='12' style='fill:#F1F1F1' stroke='black'/>",
                       "</svg><span style='color:red'> Conservative Direction&nbsp&nbsp</span>",
                       "<svg width='15' height='12'><rect width='15' height='12' style='fill:#cccccc' stroke='black'/>",
                       "</svg><span style='color:blue'> Liberal Direction&nbsp&nbsp</span>",
                       "<svg width='15' height='12'><rect width='15' height='12' style='fill:white' stroke='black'/>",
                       "</svg><span style='color:black'> Neither</span>"))
  ) %>%
  tab_style(style=cell_borders(sides=c("top","bottom"),
                               color="gray30"),
            locations=cells_body(everything())
  ) %>%
  tab_options(
    column_labels.border.bottom.color = "gray30",
    column_labels.border.top.color="gray30",
    table_body.border.bottom.color = "gray30",
    table.border.top.color = "white",
    heading.border.bottom.color = "white",
    heading.align="left",
    column_labels.padding = px(6),
    heading.title.font.size=px(24),
    heading.subtitle.font.size = px(18)
  )

gtsave(gt.by.ruling,"gt.by.ruling.png")

#### by issue area ####
gt.by.issue <- 
  ideo.r %>%
  drop_na(decisionDirection) %>%
  drop_na(issueArea) %>%
  select(caseId,issueArea,decisionDirection) %>%
  filter(issueArea %in% c(1,2,3,8,9,10)) %>%
  distinct() %>%
  tabyl(issueArea,decisionDirection) %>%
  adorn_totals(c("col","row")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits=1) %>%
  adorn_ns() %>% data.frame() %>%
  mutate(issueArea_label = c("Criminal Procedure","Economic Activity","Civil Rights",
                             "Judicial Power","First Amendment","Federalism","Overall")) %>%
  relocate(issueArea_label,.after=issueArea) %>%
  separate_wider_delim(con," (",names=c("con","con_num")) %>% 
  mutate(con=str_trim(con),con_num=str_replace(con_num,"\\)","")) %>% 
  separate_wider_delim(lib," (",names=c("lib","lib_num")) %>% 
  mutate(lib=str_trim(lib),lib_num=str_replace(lib_num,"\\)","")) %>%   
  separate_wider_delim(unk," (",names=c("unk","unk_num")) %>% 
  mutate(unk=str_trim(unk),unk_num=str_replace(unk_num,"\\)","")) %>% 
  separate_wider_delim(Total,"(",names=c(NA,"total")) %>%
  mutate(total=str_replace(total,"\\)","")) %>% 
  mutate(total2=as.numeric(total)) %>%
  arrange(desc(total2)) %>% 
  select(issueArea_label:total) %>%
  
  gt() %>%
  tab_spanner(
    label=md("**Decision Direction**"),
    columns=c(con,con_num,lib,lib_num,unk,unk_num)
  ) %>%
  #tab_spanner(label="Total Cases",columns=total) %>%
  gt_merge_stack(col1=con,col2=con_num,font_size = c("16px","15px"),
                 palette=c("red","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=lib,col2=lib_num,font_size = c("16px","14px"),
                 palette=c("blue","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=unk,col2=unk_num,font_size = c("16px","14px"),
                 palette=c("gray50","gray50"),font_weight=c("normal","normal")) %>%
  cols_label(
    issueArea_label=md("**Issue<br>Area**<br>"),
    con=html("<span style='color:red'>Conserative</span>"),
    lib=html("<span style='color:blue'>Liberal</span>"),
    unk=html("<span style='color:gray'>Neither</span>"),
    total=md("**Total<br>Cases**")
  ) %>%
  gt_highlight_rows(rows=c(6),fill="gray80") %>%
  gt_highlight_rows(rows=c(1,2,3,4,5,7),fill="#F1F1F2") %>%
  cols_width(everything()~px(100)) %>%
  #cols_align(align="center") %>%
  tab_header(
    title=html("Ideology of Decisons by Frequent Issue Areas"),
    subtitle=html( "<svg width='24' height='12'><rect width='24' height='12' style='fill:#F1F1F1' stroke='black'/>",
                   "</svg><span style='color:red'> Conservative Direction&nbsp&nbsp</span>",
                   "<svg width='24' height='12'><rect width='24' height='12' style='fill:#cccccc' stroke='black'/>",
                   "</svg><span style='color:blue'> Liberal Direction&nbsp&nbsp</span>")
  ) %>%
  tab_style(style=cell_borders(sides=c("top","bottom"),
                               color="gray30"),
            locations=cells_body(everything())
  ) %>%
  tab_options(
    column_labels.border.bottom.color = "gray30",
    column_labels.border.top.color="gray30",
    table_body.border.bottom.color = "gray30",
    table.border.top.color = "white",
    heading.border.bottom.color = "white",
    heading.align="left",
    column_labels.padding = px(6),
    heading.title.font.size=px(24),
    heading.subtitle.font.size = px(18)
  )

gtsave(gt.by.issue,"gt.by.issue.png")

#### by Justice ####
gt_by_justice <- full_join(
  (ideo.r %>%
     mutate(justiceName=str_extract(justiceName,"[A-Z][a-z]+")) %>%
     mutate(justiceName=replace(justiceName,justiceName=="Connor","O'Connor")) %>%
     drop_na(direction) %>%
     drop_na(decisionDirection) %>%
     tabyl(justiceName,direction,decisionDirection) %>%
     adorn_totals("row") %>%
     adorn_percentages("row") %>%
     adorn_pct_formatting(digits=1) %>%
     adorn_ns())$con %>% 
    data.frame(),
  (ideo.r %>%
     mutate(justiceName=str_extract(justiceName,"[A-Z][a-z]+")) %>%
     mutate(justiceName=replace(justiceName,justiceName=="Connor","O'Connor")) %>%
     drop_na(direction) %>%
     drop_na(decisionDirection) %>%
     tabyl(justiceName,direction,decisionDirection) %>%
     adorn_totals("row") %>%
     adorn_percentages("row") %>%
     adorn_pct_formatting(digits=1) %>%
     adorn_ns())$lib %>% 
    data.frame(),
  by=join_by(justiceName)
) %>%
  `colnames<-`(c("justice","con.c","lib.c","con.l","lib.l")) %>%
  
  separate_wider_delim(con.c," (",names=c("con.c","con.c_num")) %>% 
  mutate(con.c=str_trim(con.c),con.c_num=str_replace(con.c_num,"\\)","")) %>% 
  separate_wider_delim(lib.c," (",names=c("lib.c","lib.c_num")) %>% 
  mutate(lib.c=str_trim(lib.c),lib.c_num=str_replace(lib.c_num,"\\)","")) %>%
  
  separate_wider_delim(con.l," (",names=c("con.l","con.l_num")) %>% 
  mutate(con.l=str_trim(con.l),con.l_num=str_replace(con.l_num,"\\)","")) %>% 
  separate_wider_delim(lib.l," (",names=c("lib.l","lib.l_num")) %>% 
  mutate(lib.l=str_trim(lib.l),lib.l_num=str_replace(lib.l_num,"\\)","")) %>%
  
  mutate(party=c("Conservatives","Conservatives","Liberals",
                 "Liberals","Conservatives","Liberals",
                 "Conservatives","Conservatives","Conservatives",
                 "Conservatives","Conservatives","Liberals",
                 "Liberals","Liberals","Conservatives","Overall")) %>%
  
  gt(groupname_col = "party") %>%
  
  tab_spanner(
    label=md("**Conservative<br>Decisions**"),
    columns=c(con.c,con.c_num,lib.c,lib.c_num)
  ) %>%
  tab_spanner(
    label=md("**Liberal<br>Decisions**"),
    columns=c(con.l,con.l_num,lib.l,lib.l_num)
  ) %>%
  
  gt_merge_stack(col1=con.c,col2=con.c_num,font_size = c("16px","15px"),
                 palette=c("red","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=lib.c,col2=lib.c_num,font_size = c("16px","14px"),
                 palette=c("blue","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=con.l,col2=con.l_num,font_size = c("16px","15px"),
                 palette=c("red","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=lib.l,col2=lib.l_num,font_size = c("16px","14px"),
                 palette=c("blue","gray20"),font_weight=c("normal","normal")) %>%
  
  cols_label(
    justice=md("***Vote***"),
    con.c=html("<span style='color:red'><em>Conservative</em></span>"),
    lib.c=html("<span style='color:blue'><em>Liberal</em></span>"),
    con.l=html("<span style='color:red'><em>Conservative</em></span>"),
    lib.l=html("<span style='color:blue'><em>Liberal</em></span>")) %>%
  
  cols_width(everything()~px(120)) %>%
  tab_header(
    title=md("**Ideology of Decisions by Justice**")
  ) %>%
  tab_style(style=cell_borders(sides=c("top","bottom"),
                               color="gray30"),
            locations=cells_body(everything())
  ) %>%
  tab_options(
    column_labels.border.bottom.color = "gray30",
    column_labels.border.top.color="gray30",
    table_body.border.bottom.color = "gray30",
    table.border.top.color = "white",
    row_group.border.bottom.color = "black",
    row_group.border.top.color = "black",
    heading.border.bottom.color = "white",
    heading.align="left",
    column_labels.padding = px(6),
    heading.title.font.size=px(24),
    column_labels.font.size = px(18),
    table.width = px(120)
  ) %>%
  tab_style(
    style=list(cell_fill(color="blue"),
               cell_text(color="white",weight="bold")),
    locations=cells_row_groups((groups="Liberals"))
  ) %>%
  tab_style(
    style=list(cell_fill(color="red"),
               cell_text(color="white",weight="bold")),
    locations=cells_row_groups((groups="Conservatives"))
  ) %>%
  tab_style(
    style=list(cell_fill(color="black"),
               cell_text(color="white",weight="bold")),
    locations=cells_row_groups((groups="Overall"))
  ) %>%
  tab_style(
    style=list(cell_borders(
      sides="right",
      color="black",
      weight=px(1)
    )),
    locations=list(
      cells_body(
        columns=c("justice","lib.c","lib.l")
      )
    )
  ) %>%
  tab_style(
    style=list(cell_borders(
      sides=c("left","right"),
      color="black",
      weight=px(1)
    )),
    locations=list(
      cells_row_groups()
    )
  )

gtsave(gt_by_justice,"gt.by.justice2.png",expand=50)

#### by grouping ####
# prep data
vote_sorter <- function(string) {
  sapply(string, function(x) paste(sort(unlist(str_split(x,""))),collapse=""))
}

libs=c('DHSouter','EKagan','JPStevens','RBGinsburg','SGBreyer','SSotomayor')
group_votes <- ideo.r %>%
  mutate(votes=paste(majVotes,"-",minVotes)) %>%
  select(caseId,term,votes,justiceName,majority,decisionDirection) %>%
  mutate(party=ifelse(justiceName %in% libs,"L","C")) %>%
  pivot_wider(names_from=justiceName,values_from=party) %>%
  unite("maj",JPStevens:ACBarrett,sep='',na.rm=T) %>%
  mutate(maj=vote_sorter(maj)) %>% 
  drop_na(majority) %>%
  pivot_wider(names_from=majority,values_from=maj) %>%
  `colnames<-`(c("caseId","term","votes","decisionDirection","maj","min"))

xxx.gv <- group_votes %>%
  mutate(lib.votes=case_when(
    !grepl("L",maj) ~ -1,
    !grepl("L",min) ~ .75,
    grepl("L",maj) & grepl("L",min) ~ 0
  ),
  con.votes=ifelse(!grepl("C",min),.5,0),
  all.votes=lib.votes+con.votes
  )

# create table
gt.by.ideo <- rbind(cbind(xxx.gv %>% tabyl(lib.votes,decisionDirection) %>%
                            adorn_totals(c("col")) %>%
                            adorn_percentages("row") %>% 
                            adorn_pct_formatting(digits=1) %>%
                            adorn_ns() %>%
                            data.frame() %>%
                            mutate(party="Liberals") %>%
                            rename(votes=lib.votes) %>% select(-Total) %>%
                            arrange(votes),
                          xxx.gv %>% tabyl(lib.votes,decisionDirection) %>%
                            adorn_totals(c("col")) %>%
                            adorn_percentages("col") %>% 
                            adorn_pct_formatting(digits=1) %>%
                            adorn_ns() %>%
                            data.frame() %>%
                            mutate(party="Liberals") %>%
                            rename(votes=lib.votes) %>%
                            arrange(votes) %>% select(Total)),
                    cbind(xxx.gv %>% tabyl(con.votes,decisionDirection) %>%
                            adorn_totals(c("col","row")) %>%
                            adorn_percentages("row") %>% 
                            adorn_pct_formatting(digits=1) %>%
                            adorn_ns() %>%
                            data.frame() %>%
                            mutate(party="Conservatives") %>%
                            rename(votes=con.votes) %>% select(-Total) %>%
                            arrange(votes),
                          xxx.gv %>% tabyl(con.votes,decisionDirection) %>%
                            adorn_totals(c("col","row")) %>%
                            adorn_percentages("col") %>% 
                            adorn_pct_formatting(digits=1) %>%
                            adorn_ns() %>%
                            data.frame() %>%
                            mutate(party="Conservatives") %>%
                            rename(votes=con.votes) %>%
                            arrange(votes) %>% select(Total))
) %>%
  mutate(votes=c("All Dissent","Split","All Majority","Split","All Majority","Overall")) %>%
  separate_wider_delim(con," (",names=c("con","con_num")) %>% 
  mutate(con=str_trim(con),con_num=str_replace(con_num,"\\)","")) %>% 
  separate_wider_delim(lib," (",names=c("lib","lib_num")) %>% 
  mutate(lib=str_trim(lib),lib_num=str_replace(lib_num,"\\)","")) %>%   
  separate_wider_delim(unk," (",names=c("unk","unk_num")) %>% 
  mutate(unk=str_trim(unk),unk_num=str_replace(unk_num,"\\)","")) %>% 
  separate_wider_delim(Total,"(",names=c("tot","total")) %>%
  mutate(total=str_replace(total,"\\)","")) %>%
  select(-NA_) %>%
  mutate(party=ifelse(votes=="Overall","Overall",party),
         votes=ifelse(votes=="Overall","",votes)) %>%
  gt(groupname_col = "party") %>%
  tab_spanner(
    label=md("**Decision Direction**"),
    columns=c(con,con_num,lib,lib_num,unk,unk_num)
  ) %>%
  gt_merge_stack(col1=con,col2=con_num,font_size = c("16px","15px"),
                 palette=c("red","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=lib,col2=lib_num,font_size = c("16px","14px"),
                 palette=c("blue","gray20"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=unk,col2=unk_num,font_size = c("16px","14px"),
                 palette=c("gray50","gray50"),font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=tot,col2=total,font_size = c("16px",'15px'),
                 palette=c("black","black"),font_weight=c('normal','normal')) %>%
  cols_label(
    votes=md("**Group<br>Vote**"),
    con=html("<span style='color:red'>Conserative</span>"),
    lib=html("<span style='color:blue'>Liberal</span>"),
    unk=html("<span style='color:gray'>Neither</span>"),
    tot=md("**Total<br>Cases**")
  ) %>%
  cols_width(everything()~px(100)) %>%
  #cols_align(align="center") %>%
  tab_header(
    title=html("Ideology of Decisions by <span style='color:blue'>Liberal</span>/<span style='color:red'>Conserative</span> Justice Groups")
  ) %>%
  tab_style(style=cell_borders(sides=c("top","bottom"),
                               color="gray30"),
            locations=cells_body(everything())
  ) %>%
  tab_options(
    column_labels.border.bottom.color = "gray30",
    column_labels.border.top.color="gray30",
    table_body.border.bottom.color = "gray30",
    table.border.top.color = "white",
    row_group.border.bottom.color = "black",
    row_group.border.top.color = "black",
    row.striping.background_color = "#f1f1f1",
    heading.border.bottom.color = "white",
    heading.align="left",
    column_labels.padding = px(6),
    heading.title.font.size=px(24),
    heading.subtitle.font.size = px(18)
  ) %>%
  tab_style(
    style=list(cell_fill(color="blue"),
               cell_text(color="white",weight="bold")),
    locations=cells_row_groups((groups="Liberals"))
  ) %>%
  tab_style(
    style=list(cell_fill(color="red"),
               cell_text(color="white",weight="bold")),
    locations=cells_row_groups((groups="Conservatives"))
  ) %>%
  tab_style(
    style=list(cell_fill(color="black"),
               cell_text(color="white",weight="bold")),
    locations=cells_row_groups((groups="Overall"))
  )

gtsave(gt.by.ideo, "gt.by.ideo.png")

