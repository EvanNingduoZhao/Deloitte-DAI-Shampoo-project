library(shiny)
library(shiny)#
library(shinydashboard)
library(magrittr)
library(RMySQL)
library(DT)
library(scales)
library(jiebaR)
library(dplyr)
library(highcharter)
library(wordcloud2)
library(purrr)
#library(jsonlite)
library(digest)
library(ggplot2)
library(ggThemeAssist)
library(ggthemes)
library(treemapify)
library(plotly)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggrepel)
library(igraph)
library(stringr)

sales <- read.csv("sales_data.txt")
des <- read.csv("description_data.txt")
review <- read.csv("review_data.txt")
sales<-sales%>%
  rename(name.x=商品名称,price=销售价格,monthly_sale=月销量,review_amount=评论量,popularity=商品人气值)
des<-des%>%
  rename(name=商品名称,brand=品牌,officialname=型号,function.=功效,proloc=产地,tarhair=适用发质,volume=化妆品净含量)
review<-review%>%
  rename(name=商品名称,tianmaotag=天猫标签,tag_amount=标签数量,direction=舆情正负)
sales$monthly_sale<-as.numeric(sales$monthly_sale)
#des$brand<-revalue(des$brand,c("Schwarzkopf/施华蔻"="施华蔻","KERASTASE/卡诗"="卡诗","TriptychOfLune/三谷"="三谷"))
levels(des$brand)[levels(des$brand)=="Schwarzkopf/施华蔻 "] <- "施华蔻"
levels(des$brand)[levels(des$brand)=="KERASTASE/卡诗 "] <- "卡诗"
levels(des$brand)[levels(des$brand)=="TriptychOfLune/三谷 "] <- "三谷"
levels(des$brand)[levels(des$brand)=="SEEYOUNG/滋源 "] <- "滋源"
levels(des$brand)[levels(des$brand)=="L'OREAL /欧莱雅 "] <- "欧莱雅"
levels(des$brand)[levels(des$brand)=="Shiseido/资生堂 "] <- "资生堂"
levels(des$brand)[levels(des$brand)=="Moist Diane/黛丝恩 "] <- "黛丝恩"
levels(des$brand)[levels(des$brand)=="Schwarzkopf/施华蔻 "] <- "施华蔻"
levels(des$brand)[levels(des$brand)=="Watsons/屈臣氏 "] <- "屈臣氏"
levels(des$brand)[levels(des$brand)=="SHISEIDO PROFESSIONAL/资生堂专业美发 "] <- "资生堂专业美发"
levels(des$brand)[levels(des$brand)=="JANESE/珍妮诗 "] <- "珍妮诗"
levels(des$brand)[levels(des$brand)=="COCOESSENCE/遇见香芬 "] <- "遇见香芬"
levels(des$brand)[levels(des$brand)=="Adidas/阿迪达斯 "] <- "阿迪达斯"
levels(des$brand)[levels(des$brand)=="The Face Shop/菲诗小铺 "] <- "菲诗小铺"
levels(des$brand)[levels(des$brand)=="Difaso/蒂花之秀 "] <- "蒂花之秀"
levels(des$brand)[levels(des$brand)=="Dove/多芬 "] <- "多芬"
levels(des$brand)[levels(des$brand)=="Greenkosy/绿色溪谷 "] <- "绿色溪谷"
levels(des$brand)[levels(des$brand)=="COCOVEL/蔻露薇 "] <- "蔻露薇"
levels(des$brand)[levels(des$brand)=="George caroll/乔治卡罗尔 "] <- "乔治卡罗尔"
levels(des$brand)[levels(des$brand)=="REJOICE/飘柔 "] <- "飘柔"
levels(des$brand)[levels(des$brand)=="SYOSS/丝蕴 "] <- "丝蕴"
levels(des$brand)[levels(des$brand)=="Sivia/仙维娜 "] <- "仙维娜"


des_sales<-merge(des, sales, by = "id")
des_sales<-des_sales%>%
  mutate(revenue=price*monthly_sale)
sales_rank<-des_sales%>%
  group_by(brand)%>%
  summarise(totalrev=sum(revenue))%>%
  arrange(desc(totalrev))

top20brand<-as.character(head(sales_rank,20)$brand)
typeof(top20brand)

top20sales<-des_sales%>%
  filter(brand%in%top20brand)%>%
  group_by(brand)



pos_review<-merge(des,review, by = "id")%>%
  group_by(id)%>%
  filter(direction==1)%>%
  summarise(pos_amount=sum(tag_amount))

neg_review<-merge(des, review, by = "id")%>%
  group_by(id)%>%
  filter(direction==-1)%>%
  summarise(neg_amount=sum(tag_amount))

pos<-merge(des, pos_review, by = "id")

posrate_rank_bypro<-merge(pos, neg_review, by = "id")%>%
  select(id,name,brand,pos_amount,neg_amount)%>%
  mutate(posrate=pos_amount/(pos_amount+neg_amount))%>%
  arrange(desc(posrate))

posrate_rank_bybrand<-merge(pos, neg_review, by = "id")%>%
  group_by(brand)%>%
  summarise(total_pos=sum(pos_amount),total_neg=sum(neg_amount))%>%
  mutate(posrate=total_pos/(total_pos+total_neg))%>%
  arrange(desc(posrate))

shipping_removed<-review%>%
  filter(str_detect(tianmaotag, "物流")==F)%>%
  filter(str_detect(tianmaotag, "快递")==F)

with_brand<-merge(shipping_removed,des,by="id")%>%
  select(id,tianmaotag,tag_amount,direction,brand)
# revenue vs posrate by product



# revenue vs posrate by brand



top20sales_bypro<-des_sales%>%
  mutate(revenue=price*monthly_sale)%>%
  select(id,name.x,brand,function.,price,monthly_sale,revenue)%>%
  top_n(20,revenue)%>%
  arrange(desc(revenue))



top20pro_withtags<-merge(tags,top20sales_bypro,by="id")
top20pro_withtags$direction<-as.factor(top20pro_withtags$direction)
levels(top20pro_withtags$direction)[levels(top20pro_withtags$direction)==as.numeric(1)] <- "正面"
levels(top20pro_withtags$direction)[levels(top20pro_withtags$direction)==as.numeric(-1)] <- "负面"



#Top20 brand pos rating bar chart
des_sales<-merge(des, sales, by = "id")
des_sales<-des_sales%>%
  mutate(revenue=price*monthly_sale)
sales_rank<-des_sales%>%
  group_by(brand)%>%
  summarise(totalrev=sum(revenue))%>%
  arrange(desc(totalrev))

top20brand<-as.character(head(sales_rank,20)$brand)

shipping_removed<-review%>%
  filter(str_detect(tianmaotag, "物流")==F)%>%
  filter(str_detect(tianmaotag, "快递")==F)

with_brand<-merge(shipping_removed,des,by="id")%>%
  select(id,tianmaotag,tag_amount,direction,brand)

choices_proloc=c(
  '全部'='全部 ',
  '所有进口'='所有进口 ',
  '中国大陆' = '中国大陆 ', 
  '韩国' = '韩国 ',
  '日本' = '日本 ', 
  '法国' = '法国 ',
  "西班牙" = "西班牙 ",
  '美国'='美国 ',
  '英国'='英国 ',
  '澳大利亚'='澳大利亚 ',
  '德国'='德国 ',
  '泰国'='泰国 ',
  '台湾省'='台湾省 ',
  '其他'='其他 '
)
choices_tarhair=c(
  '全部'='全部 ',
  '所有发质'='所有发质 ',
  '油性发质' = '油性发质 ', 
  '干性发质' = '干性发质 ',
  '混合性发质' = '混合性发质 ', 
  '受损发质' = '受损发质 ',
  '正常发质'= '正常发质 ',
  '头皮屑'='头皮屑 '
)
ui <- dashboardPage(skin = 'black',
                    dashboardHeader(title = 'Deloitte'),
                    dashboardSidebar(sidebarMenuOutput("SidebarMenuUI")),
                    dashboardBody(# Include IntroJS styling
                      includeCSS("www/introjs.css"),
                      
                      # Include styling for the app
                      #includeCSS("app.css"),
                      
                      # Include IntroJS library
                      includeScript("www/intro.js"),
                      
                      # Include javascript code to make shiny communicate with introJS
                      includeScript("www/app.js"),
                      #                       tags$style(HTML("
                      # 
                      # 
                      # .box.box-danger>.box-body {
                      #   color:#fff;
                      #   background:#666666
                      #                     }
                      # 
                      # .box.box-solid.box-primary{
                      # border-bottom-color:#666666;
                      # border-left-color:#666666;
                      # border-right-color:#666666;
                      # border-top-color:#666666;
                      # } ")),
                      uiOutput('Body'))
)

server <- shinyServer(function(input, output, session){
  
  # sidebar menu code
  output$SidebarMenuUI <- renderMenu({
    
    sidebarMenu(
      br(),
      menuItem('行业全景', tabName = 'Competitor', icon = icon('th')),
      menuItem('热卖商品分析', tabName = 'HotSales', icon = icon('th')),
      #menuItem('热门搜索词分析', tabName = 'HotSearch', icon = icon('th')),
      #menuItem('评论抓取监视', tabName = "CommentCrawl", icon = icon("dashboard")),
      menuItem('用户偏好画像', tabName = 'CommentScore', icon = icon('th')),
      #menuItem('用户感知画像', tabName = 'CommentSentiment', icon = icon('th')),
      
      column(
        width =12,
        hr()
        #h5(icon('refresh', class = 'fa-spin'), '数据更新时间: '),
        #strong(com$update_time[1])
      )
    )
    
  })
  
  output$Body <- renderUI({
    tabItems(
      tabItem(tabName = 'Competitor',
              fluidRow(
                column(width = 1,
                       actionButton(inputId="startHelp1", label="Help", class = 'btn-success', style = 'background:#92D050; border-color:#92D050')
                ),
                # column(
                #   width = 2,
                #   checkboxInput('Outlier', '是否包括价格极高/极低的商品？', value = TRUE)
                # ),
                
                column(
                  width = 4,
                  selectizeInput('MultiCat', label = NULL,  multiple = TRUE,  
                                 choices = c('天猫' = 'ref',  '淘宝' = 'air','京东' = 'wash', '所有' = 'tv'), 
                                 selected = c('ref'), 
                                 options = list(maxItems = 4)
                  )
                )
              ),
              
              uiOutput('ComProfile')
              
      ),
      
      tabItem(tabName = 'HotSales',
              fluidRow(
                column(width = 1,
                       actionButton(inputId="startHelp2", label="Help", class="btn-success", style = 'background:#92D050; border-color:#92D050')
                )
              ),
              br(),
              fluidRow(
                box(id = 'step2_1',
                    status = 'primary',
                    title = '热卖产品排行榜', 
                    width = 8,
                    fluidRow(
                      column(width = 2,
                             selectInput('topwhat', label = "热卖前几名？",
                                         c('10' = 10, 
                                           '25' = 25,
                                           '50' = 50, 
                                           '100' = 100
                                         ),selected = 10,selectize = T)
                      ),
                      column(width = 3,
                             selectizeInput('product_loc', label = "请限定产地",multiple=T,
                                            choices= choices_proloc,selected = '全部 ')
                      ),
                      column(width = 3,
                             selectizeInput('target_hair', label = "请限定适用发质",multiple=T,
                                            choices= choices_tarhair,selected = '全部 ')
                      ),
                      dataTableOutput("top10product")
                      
                    )
                    
                ),
                box(id = 'step2_2',
                    status = 'danger',
                    title = '热卖商品关键字',
                    width = 4,
                    wordcloud2Output("wordcloud")
                )
                
              ),
              br(),
              
              fluidRow(
                uiOutput('HotSaleList')
              )
      ),
      
      
      
      tabItem(tabName = 'CommentScore',
              fluidRow(
                column(width = 1,
                       actionButton(inputId="startHelp5", label="Help", class = 'btn-success', style = 'background:#92D050; border-color:#92D050')
                ),
                
                column(
                  width = 4,
                  selectizeInput('MultiCat3', label = NULL,  multiple = TRUE,  
                                 choices = c('天猫' = 'ref',  '淘宝' = 'air','京东' = 'wash', '所有' = 'tv'), 
                                 selected = c('ref'), 
                                 options = list(maxItems = 4)
                  )
                )
              ),
              
              uiOutput('CommentBrand')
              
      )
    )  
  })
  
  
  
  observeEvent(input$product_loc,{
    if('全部 '%in%input$product_loc)
      selected_choices_loc=choices_proloc[-1][-1]
    else if ('所有进口 '%in%input$product_loc){ 
      selected_choices_loc=choices_proloc[-1][-1][-1]
    }
    else
      selected_choices_loc=input$product_loc
    updateSelectizeInput(session,'product_loc',selected = selected_choices_loc)
  })
  
  observeEvent(input$target_hair,{
    if('全部 '%in%input$target_hair)
      selected_choices_hair=choices_tarhair[-1]
    else
      selected_choices_hair=input$target_hair
    updateSelectizeInput(session,'target_hair',selected = selected_choices_hair)
  })
  
  output$price_view <- renderPlotly({
    des_sales<-merge(des, sales, by = "id")
    des_sales<-des_sales%>%
      mutate(revenue=price*monthly_sale)
    sales_rank<-des_sales%>%
      group_by(brand)%>%
      summarise(totalrev=sum(revenue))%>%
      arrange(desc(totalrev))
    
    top20brand<-as.character(head(sales_rank,20)$brand)
    typeof(top20brand)
    
    top20sales<-des_sales%>%
      filter(brand%in%top20brand)%>%
      group_by(brand)
    
    plot_ly(top20sales,color=~brand,y=~price,type="box")
  })
  
  output$market_portion <- renderPlot({
    des_sales%>%
      mutate(revenue=price*monthly_sale)%>%
      group_by(brand)%>%
      summarise(total_revenue=sum(revenue))%>%
      ggplot(aes(area=total_revenue,label=brand,fill=-total_revenue))+
      geom_treemap(alpha=0.6)+
      geom_treemap_text(fontface = "italic", colour = "white", place = "centre",family = 'STKaiti')+
      theme(legend.position = "none")
  })
  
  
  output$market_portion_pie <-renderPlotly({
    d<-des_sales%>%
      mutate(revenue=price*monthly_sale)%>%
      group_by(brand)%>%
      summarise(total_revenue=sum(revenue))%>%
      arrange(desc(total_revenue))%>%
      mutate(percent=total_revenue/sum(total_revenue))
    
    
    other<-slice(d,11:n())%>%
      summarise(brand="other",total_revenue=sum(total_revenue),percent=sum(percent))
    
    rbind(top_n(d,10),other)%>%
      plot_ly(labels = ~brand, values = ~total_revenue) %>%
      add_pie(hole = 0.6) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  ###PLOT OF TAB 1
  output$ComProfile <- renderUI({
    ui <- list(
      ref = fluidRow(
        box(id = 'step1_1',
            status = 'danger',
            title = '市场占有率Top20洗发水品牌价格全景',
            width = 12,
            plotlyOutput('price_view', height = '400px')
        ),
        box(id = 'step1_2',
            status = 'primary',
            title = '各品牌产品数量柱状图',
            width = 12,
            plotlyOutput('brand_pro_amount_bar', height = '500px')
        ),
        box(id = 'step1_4',
            status = 'primary',
            title = '洗发水TOP10品牌市场占有率Pie-Chart',
            width = 6,
            plotlyOutput('market_portion_pie', height = '300px')
        ),
        box(id = 'step1_3',
            status = 'success',
            title = '洗发水市场占有率Treemap',
            width = 6,
            plotOutput('market_portion', height = '300px')
        )
      )
    )
    

  })

  ###PLOT OF TAB 3
  output$CommentBrand <- renderUI({
    ui <- list(
      ref = fluidRow(
        box(id = 'step3_2',
            status = 'primary',
            title = '各品牌用户满意度与月营收相关性',
            width = 8,
            img(src='rate_share.png', align = "right",width = 600,
                height = 400)
        ),
        box(
          id='shihuakou_cloud',
          status = 'primary',
          title = 'Top20品牌评论标签',
          width = 4,
          wordcloud2Output('shihuakou',height = '400px')
        )
      )
    )
    
    
  })
  
  output$myImage <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = 'www/rate_share.png')
    
    # Generate the PNG
    png(outfile, width = 400, height = 300)
    hist(rnorm(input$obs), main = "Generated in renderImage()")
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$top10product <- renderDataTable({
    rank_data<-merge(des,sales,by="id")%>%
      select(name,brand,function.,proloc,tarhair,price,monthly_sale)%>%
      mutate(revenue=price*monthly_sale)
    
    levels(rank_data$proloc)[levels(rank_data$proloc)=="中国大陆 "] <- "中国 "
    levels(rank_data$proloc)[levels(rank_data$proloc)=="其他/other "] <- "其他 "
    levels(rank_data$tarhair)[levels(rank_data$tarhair)=="油性 "] <- "油性发质 "
    levels(rank_data$tarhair)[levels(rank_data$tarhair)=="干性 "] <- "干性发质 "
    inputloc<-c("中国 ","法国 ")
    rank_data%>%
      filter(proloc%in%input$product_loc)%>%
      filter(tarhair%in%input$target_hair)%>%
      top_n(as.numeric(input$topwhat),revenue)%>%
      arrange(desc(revenue))%>%
      rename(产品名称=name,品牌=brand,功能=function.,价格=price,月销量=monthly_sale,月营收=revenue)
  })
  
  
  output$wordcloud <-renderWordcloud2({
    functions<-des_sales%>%
      select(function.)
    #file.create("function_words.csv")
    write.table(functions, file = "function_words.txt",
                row.names = F, col.names = F)
    sentences<-scan("function_words.txt","character",sep="\n")
    sentences<-gsub( "[[:punct:]]", " ",sentences)
    wk<-worker()
    word_freq<-freq(wk[sentences])
    ready<-word_freq%>%
      arrange(desc(freq))
    wordcloud2(ready,color="skyblue",size=2)
  })
  
  
  output$brand_posrate <-renderPlotly({
    brand_bar_data<-merge(review,des,by="id")
    brand_bar_data$direction<-as.factor(brand_bar_data$direction)
    levels(brand_bar_data$direction)[levels(brand_bar_data$direction)==as.numeric(1)] <- "正面"
    levels(brand_bar_data$direction)[levels(brand_bar_data$direction)==as.numeric(-1)] <- "负面"
    
    pos_tag<-brand_bar_data%>%
      filter(brand%in%top20brand)%>%
      filter(direction=="正面")%>%
      group_by(brand)%>%
      summarise(pos_amount=sum(tag_amount))
    
    neg_tag<-brand_bar_data%>%
      filter(brand%in%top20brand)%>%
      filter(direction=="负面")%>%
      group_by(brand)%>%
      summarise(neg_amount=sum(tag_amount))
    
    review_by_brand<-merge(pos_tag,neg_tag,by="brand")%>%
      mutate(pos_rate=pos_amount/(pos_amount+neg_amount)*100)%>%
      arrange(desc(pos_rate))
    
    review_by_brand$pos_rate<-round(review_by_brand$pos_rate,2)
    review_by_brand$pos_rate<-as.character(review_by_brand$pos_rate)
    
    review_by_brand$pos_rate <- paste("好评率:", review_by_brand$pos_rate)
    review_by_brand$pos_rate <- paste0(review_by_brand$pos_rate,"%")
    review_by_brand$brand<-as.character(review_by_brand$brand)
    plot_ly(review_by_brand, x = ~brand, y = ~pos_amount, type = 'bar',  text = ~pos_rate,textposition='top' ,name = '正面评价',
            marker = list(color = 'rgb(158,202,225)',
            line = list(color = 'rgb(8,48,107)',width = 0.5))) %>%
      add_trace(y = ~neg_amount, name = '负面评价',marker=list(color='red')) %>%
      layout(yaxis = list(title = '评价数'), 
             xaxis=list(title='品牌'),
             barmode = 'stack')
  })
  
  output$brand_scatter <-renderPlot({
    sale_by_brand<-des_sales%>%
      group_by(brand)%>%
      summarise(total_revenue=sum(revenue))
    all_brand<-merge(posrate_rank_bybrand,sale_by_brand, by = "brand")
    top20_brand<-all_brand%>%
      filter(brand%in%top20brand)
    all_brand%>%
      ggplot(aes(x=posrate,y=total_revenue))+
      geom_point(aes(color=brand),alpha=0.5)+
      geom_point(data=top20_brand,aes(x=posrate,y=total_revenue,col=brand),alpha=1,size=2.5)+
      geom_label_repel(data=top20_brand,aes(label = brand,
                                            fill = brand), color = 'white',
                       size = 2.7,family = 'STKaiti')+
      geom_smooth(se=F)+
      theme(legend.position = "none", axis.title=element_text(family = 'STKaiti'),panel.background = element_blank())+
      labs(
        x="用户满意度",
        y="月营收")
    mtcars%>%
      ggplot(aes(x=mpg,y=cyl))+
      geom_point()
  })
  
  output$brand_pro_amount_bar <-renderPlotly({
    brand_pro_amount<-des%>%
      group_by(brand)%>%
      summarise(n=n())%>%
      arrange(desc(n))
    
    des_sales<-merge(des, sales, by = "id")
    des_sales<-des_sales%>%
      mutate(revenue=price*monthly_sale)
    
    market_total_df<-des_sales%>%
      summarise(market_total=sum(revenue))
    
    brand_share<-des_sales%>%
      mutate(total_market=market_total_df$market_total[1])%>%
      mutate(market_share=revenue/total_market)%>%
      group_by(brand)%>%
      summarise(brand_share=sum(market_share*100))%>%
      arrange(desc(brand_share))%>%
      top_n(20,brand_share)
    
    brand_share$brand<-as.character(brand_share$brand)
    
    share_pro_amount<-merge(brand_pro_amount,brand_share,by="brand")%>%
      arrange(desc(n))
    
    share_pro_amount$brand_share<-round(share_pro_amount$brand_share,2)
    share_pro_amount$brand_share<-as.character(share_pro_amount$brand_share)
    
    share_pro_amount$brand_share <- paste("市场占有率:", share_pro_amount$brand_share)
    share_pro_amount$brand_share <- paste0(share_pro_amount$brand_share,"%")
    share_pro_amount$brand<-as.character(share_pro_amount$brand)
    
    
    share_pro_amount<-share_pro_amount%>%
      rename(share=brand_share)
    
    plot_ly(share_pro_amount, x = ~brand, y = ~n, type = 'bar', text = ~share,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(
        xaxis = list(title = "品牌"),
        yaxis = list(title = "产品数量"))
  })
  
  output$shihuakou <-renderWordcloud2({
    shihuakou<-with_brand%>%
      filter(brand=="施华蔻")%>%
      select(tianmaotag,tag_amount)%>%
      group_by(tianmaotag)%>%
      summarise(total_tag_amount=sum(tag_amount))
    
    wordcloud2(shihuakou,color="skyblue",size=2)
  })
  
  output$oulaiya <-renderWordcloud2({
    oulaiya<-with_brand%>%
      filter(brand=="欧莱雅")%>%
      select(tianmaotag,tag_amount)%>%
      group_by(tianmaotag)%>%
      summarise(total_tag_amount=sum(tag_amount))
    
    wordcloud2(oulaiya,color="skyblue",size=2)
  })
  
  output$qingyang <-renderWordcloud2({
    qingyang<-with_brand%>%
      filter(brand=="CLEAR/清扬 ")%>%
      select(tianmaotag,tag_amount)%>%
      group_by(tianmaotag)%>%
      summarise(total_tag_amount=sum(tag_amount))
    
    wordcloud2(qingyang,color="skyblue",size=2)
  })
  
  output$haifeisi <-renderWordcloud2({
    haifeisi<-with_brand%>%
      filter(brand=="海飞丝 ")%>%
      select(tianmaotag,tag_amount)%>%
      group_by(tianmaotag)%>%
      summarise(total_tag_amount=sum(tag_amount))
    
    wordcloud2(haifeisi,color="skyblue",size=2)
  })
  
  output$kashi <-renderWordcloud2({
    
    kashi<-with_brand%>%
      filter(brand=="卡诗")%>%
      select(tianmaotag,tag_amount)%>%
      group_by(tianmaotag)%>%
      summarise(total_tag_amount=sum(tag_amount))
    
    wordcloud2(kashi,color="skyblue",size=2)
    
  })
  
  
})


shinyApp(ui=ui,server=server)



