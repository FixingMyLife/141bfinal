
library(shiny)
library(rvest)
library(tidyverse)
html = read_html("http://books.toscrape.com/catalogue/category/books_1/index.html")
aa = html %>% html_nodes(".nav-list ul a") %>%html_text()

ui <- fluidPage(
    titlePanel("My Bookshelf"),
    sidebarPanel(
        selectInput('book','Select the book type',c("-",trimws(aa)),selected = "-"),
        selectInput('spe','Choose a specific book to view details',c("-"),selected = '-')
    ),
 
    


        mainPanel(
            print("A table of name and price of the books"),
           tableOutput('table'),
           htmlOutput('picture'),
           print("Full Name:"),
           textOutput('text'),
           print("Product Description:"),
           textOutput('ProductDescription')
        
    )
)


server <- function(input, output,session) {
    output$table = renderTable({
        x = input$book
        booknames = list()
        bookprices = list()
        if (x!="-") {
            k = 1
            i = 1
            while (i <= 50){
                if (trimws(aa)[i] == x){break}
                i = i + 1
            }
            if ((sapply(strsplit(x, " "), length))!= 1){
                x = gsub(' ','-',x)
            }
            linkindex = i + 1
            link = gsub(' ','',paste("http://books.toscrape.com/catalogue/category/books/",x,"_",linkindex,"/index.html"))
            html = read_html(tolower(link))
            numresult = as.numeric(html%>%html_nodes("div+ strong")%>%html_text())
            if (numresult <= 20){
                booknames = html%>%html_nodes("h3")%>%html_text()
                bookprices = as.numeric(str_extract_all(html%>%html_nodes(".price_color")%>%html_text(),"\\(?[0-9,.]+\\)?")[[1]])
            }
            if(numresult > 20){
                pagenum = numresult%/%20 + 1
                i = 1
                while (i <= pagenum){
                    link = gsub(' ','',paste("http://books.toscrape.com/catalogue/category/books/",x,"_",linkindex,"/page-",i,".html"))
                    html = read_html(tolower(link))
                    pagebooknames = html%>%html_nodes("h3")%>%html_text()
                    pagebookprices = html%>%html_nodes(".price_color")%>%html_text()
                    j = 1
                    while(j - 1 < length(pagebooknames)){
                        booknames[k] = pagebooknames[j]
                        bookprices[k] = as.numeric(str_extract_all(pagebookprices[j],"\\(?[0-9,.]+\\)?")[[1]])
                        k = k + 1
                        j = j + 1
                        
                    }
                    
                    i = i + 1
                }
            }
        }
        tibble(Booknames= booknames,"Bookprices(pound£)" = bookprices)
    })
    observe({
        x = input$book
        booknames = list()
        if (x!= '-'){
            
            k = 1
            i = 1
            while (i <= 50){
                if (trimws(aa)[i] == x){break}
                i = i + 1
            }
            if ((sapply(strsplit(x, " "), length))!= 1){
                x = gsub(' ','-',x)
            }
            linkindex = i + 1
            link = gsub(' ','',paste("http://books.toscrape.com/catalogue/category/books/",x,"_",linkindex,"/index.html"))
            html = read_html(tolower(link))
            numresult = as.numeric(html%>%html_nodes("div+ strong")%>%html_text())
            if (numresult <= 20){
                booknames = html%>%html_nodes("h3")%>%html_text()

            }
            if (numresult > 20){
                pagenum =numresult%/%20 + 1
                i = 1
                while (i <= pagenum){
                    link = gsub(' ','',paste("http://books.toscrape.com/catalogue/category/books/",x,"_",linkindex,"/page-",i,".html"))
                    html = read_html(tolower(link))
                    pagebooknames = html%>%html_nodes("h3")%>%html_text()
                    
                    j = 1
                    
                    while(j - 1 < length(pagebooknames)){
                        booknames[k] = pagebooknames[j]
                        
                        k = k + 1
                        j = j + 1
                        
                    }
                    
                    i = i + 1
                }
            }
            updateSelectInput(session,"spe",label = paste("Choose a specific book to view details"),choices = c("-",booknames),selected = "-")
        }
    })
    output$picture = renderText({
        img = "https://t3.ftcdn.net/jpg/02/20/14/38/240_F_220143804_fc4xRygvJ8bn8JPQumtHJieDN4ORNyjs.jpg"
        x = input$book
        y = input$spe
        booknames = list()

        if (x!= '-'){

            k = 1
            i = 1
            while (i <= 50){
                if (trimws(aa)[i] == x){break}
                i = i + 1
            }
            if ((sapply(strsplit(x, " "), length))!= 1){
                x = gsub(' ','-',x)
            }
            linkindex = i + 1
            link = gsub(' ','',paste("http://books.toscrape.com/catalogue/category/books/",x,"_",linkindex,"/index.html"))
            html = read_html(tolower(link))
            numresult = as.numeric(html%>%html_nodes("div+ strong")%>%html_text())
            if (numresult <= 20){
                booknames = html%>%html_nodes("h3")%>%html_text()
                i = 1
                while (i<= numresult){
                    if (booknames[i] == y){break}
                    i = i + 1
                }
                urls = html%>%html_nodes(".product_pod a")%>%html_attr('href')
                destination = gsub(' ','',paste("http://books.toscrape.com/catalogue/",unlist(str_split(urls[2*i-1],'/'))[4],"/index.html"))
                desthtml = read_html(destination)
                imgu = desthtml%>%html_nodes("#product_gallery img")%>%html_attr('src')
                img = gsub(' ','',paste("http://books.toscrape.com/media/cache/",paste(unlist(str_split(imgu,'/'))[5],unlist(str_split(imgu,'/'))[6],unlist(str_split(imgu,'/'))[7],sep = '/')))



            }
            if(numresult >20){
                pagenum = numresult%/%20 + 1
                i = 1
                while (i <= pagenum){

                    link = gsub(' ','',paste("http://books.toscrape.com/catalogue/category/books/",x,"_",linkindex,"/page-",i,".html"))
                    html = read_html(tolower(link))
                    pagebooknames = html%>%html_nodes("h3")%>%html_text()
                    if (y%in%pagebooknames){break}

                    i = i + 1
                }
                j = 1
                while (j <= length(pagebooknames)){
                    if (pagebooknames[j] == y){break}
                    j = j + 1
                }
                urls = html%>%html_nodes(".product_pod a")%>%html_attr('href')
                destination = gsub(' ','',paste("http://books.toscrape.com/catalogue/",unlist(str_split(urls[2*j-1],'/'))[4],"/index.html"))
                desthtml = read_html(destination)
                imgu = desthtml%>%html_nodes("#product_gallery img")%>%html_attr('src')
                img = gsub(' ','',paste("http://books.toscrape.com/media/cache/",paste(unlist(str_split(imgu,'/'))[5],unlist(str_split(imgu,'/'))[6],unlist(str_split(imgu,'/'))[7],sep = '/')))


            }


        }
        c(
            '<img src="',img,'">'
        )




    })
    output$text = renderText({
        x = input$book
        y = input$spe
        booknames = list()
        fullname = ''
        if (x!= '-'){
            
            k = 1
            i = 1
            while (i <= 50){
                if (trimws(aa)[i] == x){break}
                i = i + 1
            }
            if ((sapply(strsplit(x, " "), length))!= 1){
                x = gsub(' ','-',x)
            }
            linkindex = i + 1
            link = gsub(' ','',paste("http://books.toscrape.com/catalogue/category/books/",x,"_",linkindex,"/index.html"))
            html = read_html(tolower(link))
            numresult = as.numeric(html%>%html_nodes("div+ strong")%>%html_text())
            if (numresult <= 20){
                booknames = html%>%html_nodes("h3")%>%html_text()
                i = 1
                while (i<= numresult){
                    if (booknames[i] == y){break}
                    i = i + 1
                }
                urls = html%>%html_nodes(".product_pod a")%>%html_attr('href')

                destination = gsub(' ','',paste("http://books.toscrape.com/catalogue/",unlist(str_split(urls[2*i-1],'/'))[4],"/index.html"))
                desthtml = read_html(destination)
                fullname = desthtml%>%html_node("h1")%>%html_text()
                
            }
            if(numresult >20){
                pagenum = numresult%/%20 + 1
                i = 1
                while (i <= pagenum){
                    
                    link = gsub(' ','',paste("http://books.toscrape.com/catalogue/category/books/",x,"_",linkindex,"/page-",i,".html"))
                    html = read_html(tolower(link))
                    pagebooknames = html%>%html_nodes("h3")%>%html_text()
                    if (y%in%pagebooknames){break}
                    
                    i = i + 1
                }
                j = 1
                while (j <= length(pagebooknames)){
                    if (pagebooknames[j] == y){break}
                    j = j + 1
                }
                urls = html%>%html_nodes(".product_pod a")%>%html_attr('href')
                destination = gsub(' ','',paste("http://books.toscrape.com/catalogue/",unlist(str_split(urls[2*j-1],'/'))[4],"/index.html"))
                desthtml = read_html(destination)
                fullname = desthtml%>%html_node("h1")%>%html_text()
                
                
            }
            prodes = desthtml%>%html_node("#product_description+ p")%>%html_text()
                
            
        }
        paste(fullname)
    })
    output$ProductDescription = renderText({
        prodes = ''
        x = input$book
        y = input$spe
        booknames = list()
        if (x!= '-'){
            
            k = 1
            i = 1
            while (i <= 50){
                if (trimws(aa)[i] == x){break}
                i = i + 1
            }
            if ((sapply(strsplit(x, " "), length))!= 1){
                x = gsub(' ','-',x)
            }
            linkindex = i + 1
            link = gsub(' ','',paste("http://books.toscrape.com/catalogue/category/books/",x,"_",linkindex,"/index.html"))
            html = read_html(tolower(link))
            numresult = as.numeric(html%>%html_nodes("div+ strong")%>%html_text())
            if (numresult <= 20){
                booknames = html%>%html_nodes("h3")%>%html_text()
                i = 1
                while (i<= numresult){
                    if (booknames[i] == y){break}
                    i = i + 1
                }
                urls = html%>%html_nodes(".product_pod a")%>%html_attr('href')
                
                destination = gsub(' ','',paste("http://books.toscrape.com/catalogue/",unlist(str_split(urls[2*i-1],'/'))[4],"/index.html"))
                desthtml = read_html(destination)
                fullname = desthtml%>%html_node("h1")%>%html_text()
                prodes = desthtml%>%html_node("#product_description+ p")%>%html_text()
            }
            if(numresult >20){
                pagenum = numresult%/%20 + 1
                i = 1
                while (i <= pagenum){
                    
                    link = gsub(' ','',paste("http://books.toscrape.com/catalogue/category/books/",x,"_",linkindex,"/page-",i,".html"))
                    html = read_html(tolower(link))
                    pagebooknames = html%>%html_nodes("h3")%>%html_text()
                    if (y%in%pagebooknames){break}
                    
                    i = i + 1
                }
                j = 1
                while (j <= length(pagebooknames)){
                    if (pagebooknames[j] == y){break}
                    j = j + 1
                }
                urls = html%>%html_nodes(".product_pod a")%>%html_attr('href')
                destination = gsub(' ','',paste("http://books.toscrape.com/catalogue/",unlist(str_split(urls[2*j-1],'/'))[4],"/index.html"))
                desthtml = read_html(destination)
                fullname = desthtml%>%html_node("h1")%>%html_text()
                prodes = desthtml%>%html_node("#product_description+ p")%>%html_text()
                
            }
            
            
            
        }
        paste(prodes)

    })
    

    
}


shinyApp(ui = ui, server = server)
