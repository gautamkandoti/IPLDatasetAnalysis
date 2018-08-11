library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)
library(lubridate)
library(plotly)



player_match=read_excel('player_match.xlsx')
player=read_excel('player.xlsx')
ball_by_ball=read_excel('ball_by_ball.xlsx')
match=read_excel('match.xlsx')
batsman_scored=read_xlsx('batsman_scored.xlsx',sheet = 1)
wicket_taken=read_xlsx('wicket_taken.xlsx')
season=read_excel("season.xlsx")
team=read_excel("team.xlsx")

match$year=year(match$match_date)

season_player_join<-left_join(season,player,by=c("orange_cap"="player_id"))

season_player_join2<-left_join(season,player,by=c("purple_cap"="player_id"))

Runs_Scored<-left_join(batsman_scored,match)

Runs_Scored$year<-year(Runs_Scored$match_date)

Wicket_taken<-left_join(wicket_taken,match)

team_win<-left_join(match,team,by=c("match_winner"="team_id"))

team_win$year=year(team_win$match_date)

team_win$year=as.character(team_win$year)
team_wise_count=data.frame()

for(i in (unique(team_win$year))){
  team_wise_count=rbind(team_wise_count,tail(team_win %>% filter(year==i) %>% 
                                               select(match_id,team_name,year),1))    }

firstjoin=left_join(batsman_scored,ball_by_ball)

result1=firstjoin %>% group_by(striker) %>% summarise(Runs=sum(runs_scored))

secondjoin=left_join(result1,player,by=c("striker"="player_id"))

l=left_join(player,player_match)

v=l %>% group_by(player_id,player_name) %>% summarise(count=n())

result2=firstjoin %>% group_by(striker) %>% summarise(Runs=sum(runs_scored))

thirdjoin=left_join(result2,v,by=c("striker"="player_id"))

thirdjoin$BattingAverage=round(thirdjoin$Runs/thirdjoin$count)

strikerate=firstjoin %>% group_by(striker) %>% summarise(Balls=n())

fourthjoin=left_join(strikerate,player,by=c("striker"="player_id"))

fourthjoin$runs<-result1$Runs

fourthjoin %>% select(player_name,Balls,runs)

fourthjoin$Strikerate<-((fourthjoin$runs/fourthjoin$Balls)*100)


##Header of Dashboard

header=dashboardHeader(title="IPL Dashboard")

sidebar=dashboardSidebar(selectInput(inputId = "Year",
                                     label = "Select the Year",
                                     choices = c('All',season_player_join$season_year),
                                     selected = "All"),
                         sliderInput("TopN",label = "Number of Batsman",min = 0,max = 50,value = 5),
                         sidebarMenu(menuItem("Dashboard",tabName = "KPI",icon = icon("Dasboard")),
                                     menuItem("Graphs",tabName = "RowData",icon = icon("Graphs")),
                                     menuItem("Title Win",tabName = "Winners",icon = icon("Title Win")),
                                     menuItem("Caps",tabName = "Capholders",icon = icon("Caps")),
                                     menuItem("Toss & Match Winner",tabName="Toss_Match",icon = icon("Toss & Match Winner"))))

##Body of the Dashboard

body=dashboardBody(
  tabItems(
    tabItem(
      tabName = "KPI",
      fluidRow(
        valueBoxOutput("value3",width = 3),
        valueBoxOutput("value4",width = 3),
        valueBoxOutput("value5",width = 3),
        valueBoxOutput("value6",width = 3))),
        
    tabItem(
      tabName = "RowData",
      fluidRow(
        box(title = "Top N Batsman",width = 6,collapsible = T,
                   plotlyOutput("TopBatsman")),
        box(title = "Top Batting Average",width = 6,collapsible = T,
                    plotlyOutput("TopAverage"))),
      fluidRow(
        box(title = "Top Strike Rate",width = 6,collapsible = T,
                    plotlyOutput("TopStrikeRate")),
      
        box(title = "TOp Highest Score",width = 6,collapsible = T,
                    plotlyOutput("TopScorer")))),
    tabItem(
      tabName = "Winners",
      fluidRow(
        box(title = "Title Winners",width = 12,collapsible = T,
            plotlyOutput("TitleStriker"))
      )
    ),
    tabItem(
      tabName = "Capholders",
      fluidRow(
        box(title = "Orange Cap Holders",width = 6,collapsible = T,DTOutput("orange")),
        box(title = "Purple Cap Holders",width = 6,collapsible = T,DTOutput("purple"))
      )
    ),
    tabItem(
      tabName = "Toss_Match",
      fluidRow(
        box(title = "Toss and Match Wins By Team",width = 12,collapsible = T,
            plotlyOutput("Toss_Match_Winners"))
      )
    )))

##Lets create a UI

UI=dashboardPage(header = header,sidebar = sidebar,body = body)

server=function(input,output){
  output$value3=renderValueBox({
    data<-{
      if(input$Year=="All"){
        data1<-Runs_Scored %>% filter(runs_scored==6) %>% summarise(Sixes=n())
      }else{
        data1<-Runs_Scored %>% filter(runs_scored==6 & year==input$Year) %>%  
          summarise(Sixes=n())
      }
      data1
      }
      Six=data %>% select('Sixes')
      valueBox(Six,"Sixes Per Season",color = "green")
     })
  
  output$value4=renderValueBox({
    data<-{
      if(input$Year=="All"){
        data1<-Runs_Scored %>% filter(runs_scored==4) %>% summarise(Fours=n())
      }else{
          data1<-Runs_Scored %>% filter(runs_scored==4 & year==input$Year) %>% 
            summarise(Fours=n())
        }
      data1
      }
    Four=data %>% select('Fours')
    valueBox(Four,"Fours Per Season",color = "red")
    })
  
  output$value5=renderValueBox({
    data<-{
      if(input$Year=="All"){
        data1<-match
      }else{
       data1<-filter(match,year==input$Year)
      }
      data1
      }
      valueBox(length(unique(data$match_id)),"Number of Matches",color = "orange")
     })
  
  output$value6=renderValueBox({
    data<-{
      if(input$Year=="All"){
        data1<-length(Wicket_taken$match_id)
      }else{
        data1<-Wicket_taken %>% filter(year==input$Year) %>% summarise(Wickets=n())
      }
    data1
      }
    valueBox(data,"No.of Wickets")
     })
  
  output$TopBatsman=renderPlotly({
    batsman_top_n=secondjoin %>% select(Runs,player_name) %>% arrange(-Runs)  %>%
    head(input$TopN) %>% ggplot(aes(x=reorder(player_name,-Runs),y=Runs,fill=player_name))+
      geom_bar(stat = 'identity')+theme(axis.text.x = element_blank())+xlab('Player Name')+ylab('Runs')+theme_bw()
    
    ggplotly(batsman_top_n)
    
  })
  
  output$TopAverage=renderPlotly({
    batsman_batting_average<-thirdjoin %>% select(player_name,BattingAverage) %>% 
      arrange(-BattingAverage) %>% head(input$TopN) %>% ggplot(aes(x=reorder(player_name,-BattingAverage),y=BattingAverage,fill=player_name))+
      geom_bar(stat = "identity")+theme(axis.title.x = element_blank())+xlab('Player Name')+ylab('Batting Average')+theme_bw()
    
    ggplotly(batsman_batting_average)
    
    })
   
  output$TopStrikeRate=renderPlotly({
    Batting_Strike_Rate<-fourthjoin %>% select(player_name,Strikerate) %>% arrange(-Strikerate) %>% 
      head(input$TopN) %>% ggplot(aes(x=reorder(player_name,-Strikerate),y=Strikerate,fill=player_name))+
      geom_bar(stat="identity")+theme(axis.title.x = element_blank())+xlab('Player Name')+ylab('Strike Rate')+theme_bw()
  
    ggplotly(Batting_Strike_Rate)
    })
  
  output$TitleStriker=renderPlotly({
    a<-team_wise_count %>% group_by(team_name,year) %>% summarise(count=as.character(n()))
    
    b<-ggplot(a,aes(x=year,y=count,fill=team_name),fill=team_name)+
      geom_bar(stat="identity")
    
    ggplotly(b)
  })
  output$TopScorer=renderPlotly({
    v<-firstjoin %>% group_by(match_id,striker) %>% summarise(Run=sum(runs_scored))
    k=left_join(v,player,by=c("striker"="player_id"))
    p<-k %>% arrange(-Run) %>% head(10) %>% ggplot(aes(x=reorder(player_name,Run),y=Run,fill=player_name))+
      geom_bar(stat = "identity")+xlab('Player Name')+ylab("Runs")
    
    ggplotly(p)
  })
  
  output$orange=renderDT({
    orange_cap<-season_player_join %>% select(season_year,player_name)
    datatable(orange_cap)
  })
  
  output$purple=renderDT({
    purple_cap<-season_player_join2 %>% select(season_year,player_name)
    datatable(purple_cap)
  })
  
  output$Toss_Match_Winners=renderPlotly({
    toss<-left_join(match,team,by=c("toss_winner"="team_id"))
    toss_winner<-toss %>% group_by(team_name) %>% summarise(Wins=n())
    toss_winner$type="toss_winner"
    Winner<-left_join(match,team,by=c("match_winner"="team_id"))
    Match_Winner<-Winner %>% group_by(team_name) %>% summarise(Wins=n())
    Match_Winner$type<-"Match_Winner"
    toss_match<-rbind(toss_winner,Match_Winner)
    conclusion<-toss_match %>% group_by(team_name,type) %>% summarise(wins=sum(Wins)) %>% 
      ggplot(aes(x=team_name,y=wins,colour=type,fill=type))+
      geom_bar(stat="identity",position="dodge")+
      theme(axis.text.x =element_text(angle = 90))+
      xlab('Toss and Match Winners')
    
    ggplotly(conclusion)
  })
  
  }
  
shinyApp(UI,server)
