install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("scales")
install.packages("DT")
install.packages("knitr")
install.packages("plotly")
install.packages(("shiny"))                 
install.packages("flexdashboard")
install.packages("learningCurve")
install.packages("ggrepel")
install.packages("highcharter")
install.packages("RColorBrewer")
install.packages("countrycode")
# Import các gói 
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(plotly)
library(zoo)
library(countrycode)
library(highcharter)
library(data.table)
library(tibble)
library(DT)
library(knitr)
library(stringr)
library(tidyverse)
library(magrittr)
library(ggrepel)
library(RColorBrewer)


#-------------------------------------------------------------------------------
#CHAPTER1

# Lấy dữ liệu từ file excel
excel_file_path_chapter_1 <- "CHAPTER1.xlsx"
all_sheet_names_chapter_1 <- excel_sheets(excel_file_path_chapter_1)
print(all_sheet_names_chapter_1)

#-------------------------------------------------------------------------------
#1.1 Câu chuyện về cây lúa: Đông Nam Á phải thu hẹp khoảng cách năng suất để tiếp tục là vựa lúa lớn 
#Biểu đồ Xu hướng năng suất trung bình của ASEAN (1990-2022)
data <- read_excel(excel_file_path_chapter_1, sheet = "rice_harvested")
harvested_data <- filter(data, Element == 'Yield')

harvested_data <- harvested_data %>%
  select(Area, Year, Value) %>%
  pivot_wider(names_from = Year, values_from = Value)

last_col <- tail(names(harvested_data), n = 1)
harvested_data <- harvested_data %>%
  arrange(desc(get(last_col)))

country_colors <- c("Cambodia" = "#514E90", "Indonesia" = "#F5AF72", "Myanmar" = "#DA5559", "Philippines" = "#E5842D", "Thailand" = "#305032", "Viet Nam" = "#0E0E0C")
country_shapes <- c("Cambodia" = 16, "Indonesia" = 17, "Myanmar" = 18, "Philippines" = 19, "Thailand" = 15, "Viet Nam" = 8)

harvested_long <- harvested_data %>%
  pivot_longer(cols = -Area, names_to = 'Year', values_to = 'Value') %>%
  mutate(Year = as.numeric(Year), 
         Value = Value * 100 / 1e6,  
         text = paste("Year: ", Year, "<br>Area: ", Area, "<br>Value: ", Value))
#plot
gg <- ggplot(harvested_long, aes(x = Year, y = Value, group = Area, color = Area, shape = Area, text = text)) +
  geom_line() +
  geom_point(size = 1.75) + # Điều chỉnh kích thước của điểm
  scale_color_manual(values = country_colors) +
  scale_shape_manual(values = country_shapes) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"), # Đường kẻ ngang mờ
        panel.grid.minor = element_blank(), # Loại bỏ gridlines phụ
        axis.line = element_line()) + # Giữ lại đường kẻ trục
  labs(title = "Xu hướng năng suất trung bình của ASEAN (1990-2022)",
       x = "Năm",
       y = "Tấn/ha") + # Update y-axis label
  theme(legend.position = "right")
# Biến đổi biểu đồ thành biểu đồ tương tác và gán vào một biến
interactive_plotY <- ggplotly(gg, tooltip = c("text"))
interactive_plotY <- interactive_plotY %>% 
  layout(hoverinfo = "none",
         legend = list(orientation = "h", 
                       x = 0.5, y = -0.15, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "center",  
                       traceorder = "normal",  
                       itemsizing = "constant",  
                       itemwidth = 50  
         )) 
# In biểu đồ tương tác
interactive_plotY
#-------------------------------------------------------------------------------
#Biểu đồ Xu hướng diện tích thu hoạch lúa của ASEAN (1990-2022)
harvested_data <- filter(data, Element == 'Area harvested')
harvested_data <- harvested_data %>%
  select(Area, Year, Value) %>%
  pivot_wider(names_from = Year, values_from = Value)

last_col <- tail(names(harvested_data), n = 1)
harvested_data <- harvested_data %>%
  arrange(desc(get(last_col)))
country_colors <- c("Cambodia" = "#514E90", "Indonesia" = "#F5AF72", "Myanmar" = "#DA5559", "Philippines" = "#E5842D", "Thailand" = "#305032", "Viet Nam" = "#0E0E0C")
country_shapes <- c("Cambodia" = 16, "Indonesia" = 17, "Myanmar" = 18, "Philippines" = 19, "Thailand" = 15, "Viet Nam" = 8)

harvested_long <- harvested_data %>%
  pivot_longer(cols = -Area, names_to = 'Year', values_to = 'Value') %>%
  mutate(Year = as.numeric(Year), Value = Value / 1e6, # Convert to million ha
         text = paste("Year: ", Year, "<br>Area: ", Area, "<br>Value: ", Value))

hh <- ggplot(harvested_long, aes(x = Year, y = Value, group = Area, color = Area, shape = Area, text = text))  +
  geom_line(aes(color = Area)) +
  geom_point(aes(color = Area, shape = Area), size = 3) +
  scale_color_manual(values = country_colors) +
  scale_shape_manual(values = country_shapes) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"), # Đường kẻ ngang mờ
        panel.grid.minor = element_blank(), # Loại bỏ gridlines phụ
        axis.line = element_line()) + # Giữ lại đường kẻ trục
  labs(title = "Xu hướng diện tích thu hoạch lúa của ASEAN (1990-2022)",
       x = "Năm",
       y = "Triệu ha") + 
  theme(legend.position = "right")

# Biến đổi biểu đồ thành biểu đồ tương tác và gán vào một biến
interactive_plotS <- ggplotly(hh, tooltip = c("text"))
interactive_plotS <- interactive_plotS %>% 
  layout(hoverinfo = "none",
         legend = list(orientation = "h", 
                       x = 0.5, y = -0.15, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "center",  
                       traceorder = "normal",  
                       itemsizing = "constant",  
                       itemwidth = 50  
         )) 
# In biểu đồ tương tác
interactive_plotS
#-------------------------------------------------------------------------------
# Đọc dữ liệu từ file Excel
data <- read_excel(excel_file_path_chapter_1, sheet = "rice_yield")

# Tính phần trăm khoảng cách năng suất
data$YieldGapPercent <- (data$`Yield gap` / data$`Yield potential`) * 100

# Biến đổi dữ liệu thành dạng dài
data_long <- data %>%
  pivot_longer(cols = c("Average farmer yield", "Yield gap"), names_to = "Type", values_to = "Value")

# Vẽ biểu đồ cơ bản
tt <- ggplot(data_long, aes(x = Country, y = Value, fill = Type)) +
  geom_bar(stat = "identity", color = "white", alpha = 1, width = 0.6) +
  geom_text(aes(label = ifelse(Type == "Yield gap", paste0(round(YieldGapPercent, 1), "%"), "")),
            position = position_stack(vjust = 0.6), color = "black") +
  scale_fill_manual(values = c("Average farmer yield" = "#514E90", "Yield gap" = "#D8B370")) +
  scale_y_continuous(name = "Tấn/ha", labels = scales::comma) +
  labs(x = "Quốc gia", title = "Biểu đồ thể hiện Tiềm năng năng suất trung bình của các quốc gia ASEAN") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(), 
        axis.text.x = element_text(angle = 360, hjust = 1, size = 8),
        plot.title = element_text(size = 9.4, face = "bold"),
        legend.position = "right") # Đặt chú thích fill sang bên phải

# Tạo biểu đồ tương tác với kích thước đã điều chỉnh
interactive_plotPotential <- ggplotly(tt, width = 700, height = 400)

# In biểu đồ tương tác
interactive_plotPotential

#---------------------------------------------------------------------------------
#Biểu đồ thể hiện Xu hướng tỷ lệ tự cung tự cấp lúa gạo của các nước ASEAN
data <- read_excel(excel_file_path_chapter_1, sheet = "rice_ssr")
ssr_plot <- ggplot(data, aes(x = Country, y = SSR)) +
  geom_bar(stat = "identity", fill = "#002379", alpha = 0.9, width = 0.5, color = "white") +
  labs(title = "Xu hướng tỷ lệ tự cung tự cấp lúa gạo của các nước ASEAN", x = "Quốc gia", y = "SSR") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#FF5F00", size = 0.7) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"), # Đường kẻ ngang mờ
        panel.grid.minor = element_blank(), # Loại bỏ gridlines phụ
        axis.line = element_line()) + # Giữ lại đường kẻ trục 
  theme(axis.text.x = element_text(angle = 360, hjust = 1, size = 8),
        plot.title = element_text(size = 12, face = "bold")  # Điều chỉnh kích thước và in đậm tiêu đề
  )

# Biến đổi thành biểu đồ tương tác
ssr_plot_interactive <- ggplotly(ssr_plot, width = 560, height = 400)

# Thêm phần chú thích fill ở dưới
ssr_plot_interactive <- ssr_plot_interactive %>% 
  layout(legend = list(orientation = "h", 
                       x = 0.25, y = -0.18, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1))
# Gán biểu đồ tương tác vào một biến riêng
ssr_plot_interactive
#-------------------------------------------------------------------------------
# Biểu đồ thặng dư gạo của mỗi quốc gia ASEAN
data <- read_excel(excel_file_path_chapter_1, sheet = "rice_ssr")
ricesunplus_plot <- ggplot(data, aes(x = Country, y = Ricesunplus)) +
  geom_bar(stat = "identity", fill = "#FC4100", alpha = 0.8, width = 0.5, color = "white") + # Thêm viền đen
  labs(title = "Thặng dư gạo của mỗi quốc gia", x = "Quốc gia", y = "Triệu tấn") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#002379", size = 0.7) + # Thay đổi màu của đường y = 0 thành màu đỏ
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"), # Đường kẻ ngang mờ
        panel.grid.minor = element_blank(), # Loại bỏ gridlines phụ
        axis.line = element_line()) + # Giữ lại đường kẻ trục 
  theme(axis.text.x = element_text(angle = 360, hjust = 1, size=7.8))

# Chuyển đổi sang biểu đồ tương tác
ricesunplus_interactive <- ggplotly(ricesunplus_plot, width = 560, height = 400)

# In ra biểu đồ tương tác
ricesunplus_interactive

#---------------------------------------------------------------------------------
#1.1.2Năng lực cạnh tranh của chuối ở khu vực Đông Nam Á
#Biểu đồ tỷ lệ Đất nông nghiệp của các quốc gia
data <- read_excel(excel_file_path_chapter_1, sheet = "agricultural_land")

# Vẽ biểu đồ
agricultural_land_plot <- ggplot(data, aes(x = Year, y = Value, fill = `Country or Area`)) + 
  geom_bar(stat = "identity", position = "dodge", color = "white", alpha = 0.8, width = 0.8) + 
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(), 
        axis.text.x = element_text(angle = 360, hjust = 1, size = 8.5)) +
  labs(title = "Biểu đồ tỷ lệ Đất nông nghiệp", 
       x = "Năm", 
       y = "%", 
       fill = "Quốc gia") +
  scale_fill_manual(values = c("#0C2D57", "#FC6736", "#B31312", "#F7B787"))

# Chuyển sang biểu đồ tương tác
agricultural_land_interactive <- ggplotly(agricultural_land_plot, width = 600, height = 400)

# In ra biểu đồ tương tác
agricultural_land_interactive

#-------------------------------------------------------------------------------
#Biểu đồ năng suất trái cây nhiệt đới
fruit <- read_excel(excel_file_path_chapter_1, sheet = "fruit")
fruit <- subset(fruit, Item %in% c("Bananas", "Coconuts, in shell", "Mangoes, guavas and mangosteens"))
fruit$Year_Group <- factor(fruit$Year)

# Vẽ biểu đồ
fruit_plot <- ggplot(fruit, aes(x = Area, y = Value, fill = Item)) +
  geom_bar(stat = "identity", position = "stack", alpha = 1, width = 0.8) +  
  labs(title = "Năng suất của trái cây nhiệt đới ASEAN (2015-2022)",
       x = "Quốc gia", y = "Tấn",
       fill = "Loại trái cây") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5),
        panel.background = element_blank()) +
  facet_wrap(~Year_Group, nrow = 1, scales = "free_x") +  
  scale_fill_manual(values = c("#002379", "#FF5F00", "#FF9F66")) +  # Màu fill tùy chỉnh
  scale_y_continuous(labels = function(x) x * 100 / 1000000, name = "Năng suất (tấn/ha)") +
  scale_x_discrete("Quốc gia")

# Chuyển sang biểu đồ tương tác
fruit_interactive <- ggplotly(fruit_plot, width = 750, height = 400)

# In ra biểu đồ tương tác
fruit_interactive
#--------------------------------------------------------------------------
export <- read_excel(excel_file_path_chapter_1, sheet = "banana_export")

# Sắp xếp dữ liệu và tính toán tốc độ
export <- export %>%
  arrange(Area, Year) %>%
  group_by(Area) %>%
  mutate(Speed = (Value - lag(Value))/lag(Value) * 100) %>%
  ungroup()

# Vẽ biểu đồ
export_plot <- ggplot(export, aes(x = Year, y = Speed, color = Area)) +
  geom_line(size = 1) +
  labs(title = "Tốc độ xuất khẩu của 4 quốc gia",
       x = "Năm",
       y = "%",
       color = "Quốc gia") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(angle = 360, hjust = 1, size = 8.5)) + # Điều chỉnh nhãn trục x
  scale_color_manual(values = c("#30609B", "#DB4D33", "#F4C138", "#6B8567")) # Điều chỉnh màu sắc

# Chuyển sang biểu đồ tương tác
export_interactive <- ggplotly(export_plot, width = 600, height = 400)

# Thêm phần chú thích fill ở dưới
export_interactive <- export_interactive %>% 
  layout(legend = list(orientation = "h",
                       x = 0.06, y = -0.15,
                       bgcolor = "white",
                       bordercolor = "black",
                       borderwidth = 1))

# In ra biểu đồ tương tác
export_interactive

#-------------------------------------------------------------------------------
#1.1.3. Cao su - tác nhân của nạn phá rừng ở các quốc gia Đông Nam Á 
#Biểu đồ diện tích mất rừng liên quan đến cao su(2001-2016) ở ASEAN
rubber_data <- read_excel(excel_file_path_chapter_1, sheet = "rubber_deforestation")
rubber_data$total_kba <- rubber_data$`Inside KBA` + rubber_data$`Outside KBA`
max_deforestation <- max(rubber_data$`Deforestation 2001-2016 (ha)`)
rubber_data$percentage_scaled <- rubber_data$`% of Rubber relating to deforestation 2001-2016` * max_deforestation
rubber_data$fill_color <- ifelse(rubber_data$`Inside KBA` > 0 | rubber_data$`Outside KBA` > 0, "orange", "blue")

rubber_plot <- ggplot(rubber_data, aes(x = reorder(Country, -`Deforestation 2001-2016 (ha)`), 
                                       text = paste("Country: ", Country, "<br>",
                                                    "Deforestation area (ha): ", total_kba, "<br>",
                                                    "Rate of Rubber relating to deforestation 2001-2016: ", percentage_scaled))) +
  geom_bar(aes(y = total_kba, fill = Country), stat = "identity", position = "stack") +
  geom_line(aes(y = percentage_scaled, group = 1), color = "black") +
  geom_point(aes(y = percentage_scaled), color = "black") +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"), # Đường kẻ ngang mờ
        panel.grid.minor = element_blank(), # Loại bỏ gridlines phụ
        axis.line = element_line(), # Giữ lại đường kẻ trục
        axis.text.x = element_text(angle = 360, hjust = 1, size = 7.5), # Xoay nhãn trục x một góc 360 độ
        plot.title = element_text(size = 6.0, face = "bold"), # Điều chỉnh kích thước và in đậm tiêu đề
        legend.position = "bottom") + # Đặt chú thích fill ở dưới
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"), # Đường kẻ ngang mờ
        panel.grid.minor = element_blank(), # Loại bỏ gridlines phụ
        axis.line = element_line()) + # Giữ lại đường kẻ trục
  labs(x = "Quốc gia", y = "ha", title = "Diện tích mất rừng liên quan đến cao su (2001-2016)") +
  scale_y_continuous(sec.axis = sec_axis(~./max_deforestation, name = "Rate of Rubber relating to deforestation 2001-2016")) +
  scale_fill_manual(values = c("#FF5F00", "#002379", "#8DECB4", "#514E90", "#FF7F00", "#F4C138", "#FF9F66", "#DF56A8", "#41B06E"))

rubber_interactive <- ggplotly(rubber_plot, tooltip = "text", width = 700, height = 400)

rubber_interactive


#-------------------------------------------------------------------------------
# Biểu đồ thể hiện nạn phá rừng và giá cao su qua các năm của ASEAN
data <- read_excel(excel_file_path_chapter_1, sheet = "rubber_price")
data_long <- data %>%
  gather(key = "Country", value = "Deforestation", -Year, -`Rubber price (US cents per pound)`)

# Create a new color palette using RColorBrewer
new_palette <- brewer.pal(n = 8, name = "Spectral") 

# Create the column and line chart
pr <- ggplot() +
  geom_col(data = data_long %>% filter(Country != "Total deforestation"), aes(x = Year, y = Deforestation, fill = Country), width = 0.5) +
  geom_line(data = data, aes(x = Year, y = `Rubber price (US cents per pound)`*10, color = "Rubber Price (US cents per pound)"), stat="identity") +
  scale_fill_manual(values = new_palette) +
  scale_color_manual(values = "blue", name = "") + 
  scale_y_continuous(
    name = "K.ha/10",
    breaks = scales::pretty_breaks(n = 5),
    sec.axis = sec_axis(~./10, name = "Rubber Price", breaks = scales::pretty_breaks(n = 5))
  ) +
  labs(
    title = "Nạn phá rừng và giá cao su qua các năm của ASEAN (1993-2016)",
    x = "Năm"
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"), # Đường kẻ ngang mờ
        panel.grid.minor = element_blank(), # Loại bỏ gridlines phụ
        axis.line = element_line(), # Giữ lại đường kẻ trục
        axis.text.x = element_text(angle = 360, hjust = 1),
        plot.title = element_text(size = 9, face = "bold")
  )

pr <- ggplotly(pr, tooltip = c("x", "y"), width = 700, height = 400)  

# Đặt chú thích sang bên phải dưới dạng một hàng dọc
pr <- pr %>%
  layout(
    legend = list(
      orientation = "v",    # Chú thích dọc
      x = 1,             # Đặt vị trí x ngoài biểu đồ
      y = 0.5,              # Đặt vị trí y ở giữa
      yanchor = "middle",   # Chú thích ở giữa theo trục y
      xanchor = "left",     # Đặt chú thích bên phải của biểu đồ
      tracegroupgap = 5     # Khoảng cách giữa các nhóm
    )
  )

pr





#---------------------------------------------------------------------------------
#CHĂN NUÔI
data <- read_excel(excel_file_path_chapter_1, sheet = "animal_husbandry")

# Lọc dữ liệu
filtered_data <- data %>%
  filter(Element == "Producing Animals/Slaughtered",
         Item == "Meat of pig with the bone, fresh or chilled",
         Area %in% c("Viet Nam", "Thailand", "Philippines", "Indonesia"))

# Tạo biểu đồ cột
gm <- ggplot(filtered_data, aes(x = Year, y = Value, fill = Area)) +
  geom_col(position = "dodge") +
  labs(title = "Giết mổ lợn hàng năm",
       x = "Năm",
       y = "Con",
       fill = "Quốc gia") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        panel.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("Viet Nam" = "#377EB8", 
                               "Thailand" = "#F4C138", 
                               "Philippines" = "#6B8567", 
                               "Indonesia" = "#DB4D33"))

# Chuyển đổi sang biểu đồ tương tác
gm <- ggplotly(gm, tooltip = c("x", "y", "fill"), width = 600, height = 500)

# Thêm phần chú thích fill ở dưới
gm <- gm %>% 
  layout(legend = list(orientation = "h", 
                       x = 0, y = -0.2, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "left", 
                       traceorder = "normal", 
                       tracegroupgap = 30))

gm


#Lọc dữ liệu
data <- read_excel(excel_file_path_chapter_1, sheet = "animal_husbandry")

filtered_data <- data %>%
  filter(Element == "Production",
         Item == "Meat of pig with the bone, fresh or chilled",
         Area %in% c("Viet Nam", "Thailand", "Philippines", "Indonesia"))

# Vẽ biểu đồ cột đứng
sl <- ggplot(filtered_data, aes(x = Year, y = Value, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sản lượng thịt lợn hằng năm",
       x = "Năm",
       y = "Tấn",
       fill = "Quốc gia") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("Viet Nam" = "#377EB8", 
                               "Thailand" = "#F4C138", 
                               "Philippines" = "#6B8567", 
                               "Indonesia" = "#DB4D33"))

# Chuyển đổi sang biểu đồ tương tác
sl <- ggplotly(sl, tooltip = c("x", "y", "fill"), width = 600, height = 500)

# Thêm phần chú thích fill ở dưới
sl <- sl %>% 
  layout(legend = list(orientation = "h", 
                       x = 0.05, y = -0.2, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "left", 
                       traceorder = "normal", 
                       tracegroupgap = 30))

sl

#Lọc dữ liệu
data <- read_excel(excel_file_path_chapter_1, sheet = "animal_husbandry")
# Lọc dữ liệu
filtered_data <- data %>%
  filter(Element == "Stocks",
         Item == "Swine / pigs",
         Area %in% c("Viet Nam", "Thailand", "Philippines", "Indonesia"))

dl <- ggplot() +
  geom_line(data = filtered_data, aes(x = Year, y = Value, color = Area), size = 0.7) +
  labs(title = "Đàn lợn của một số nước thuộc Đông Nam Á",
       x = "Năm",
       y = "Con",
       color = "Quốc gia") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14),  # Điều chỉnh kích thước tiêu đề
        axis.title = element_text(size = 12),  # Điều chỉnh kích thước các nhãn trục
        axis.text = element_text(size = 10)) +  # Điều chỉnh kích thước các số trên trục
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_manual(values = c("Viet Nam" = "#377EB8", 
                                "Thailand" = "#F4C138", 
                                "Philippines" = "#6B8567", 
                                "Indonesia" = "#DB4D33"))

# Chuyển sang biểu đồ tương tác và điều chỉnh kích thước
dl <- ggplotly(dl, tooltip = c("x", "y", "color"), width = 700, height = 400)

# In ra biểu đồ tương tác
dl


#Lọc dữ liệu
data <- read_excel(excel_file_path_chapter_1, sheet = "animal_husbandry")
filtered_data <- data %>%
  filter(Element == "Production",
         Item %in% c("Meat of chickens, fresh or chilled", "Meat of ducks, fresh or chilled"),
         Area %in% c("Viet Nam", "Thailand", "Lao People's Democratic Republic", "Malaysia"),
         Year >= 2010 & Year <= 2022)

# Tạo một bản sao của dữ liệu đã lọc
filtered_data_copy <- filtered_data
selected_years <- seq(2010, 2022, by = 3)
# Thay đổi giá trị fill cho các loại thịt
filtered_data_copy$Item <- ifelse(filtered_data_copy$Item == "Meat of chickens, fresh or chilled", "Thịt gà tươi hoặc ướp lạnh", "Thịt vịt tươi hoặc ướp lạnh")

# Vẽ biểu đồ cột chồng
gv <- ggplot(filtered_data_copy, aes(x = Year, y = Value, fill = Item)) +
  geom_col(position = "stack") +
  facet_wrap(~ Area, scales = "free_y") +
  labs(title = "Sản lượng thịt gà và vịt",
       y = "Tấn",
       fill = "Loại thịt") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks = selected_years) + # Chỉ hiển thị các năm đã chọn
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("Thịt gà tươi hoặc ướp lạnh" = "#377EB8", "Thịt vịt tươi hoặc ướp lạnh" = "#DB4D33"))

# Chuyển đổi thành biểu đồ tương tác
gv <- ggplotly(gv, width = 550, height = 400)
# Thêm phần chú thích fill ở dưới
gv <- gv %>% 
  layout(legend = list(orientation = "h", # Đặt chú thích fill ở dưới
                       x = 0, y = -0.15, # Điều chỉnh vị trí của chú thích fill
                       bgcolor = "white", # Màu nền của chú thích fill
                       bordercolor = "black", # Màu viền của chú thích fill
                       borderwidth = 1,
                       xanchor = "left", # Căn chỉnh vị trí theo trục x bên trái
                       traceorder = "normal", # Xác định thứ tự xuất hiện của các mục trong chú thích fill
                       tracegroupgap = 20), # Điều chỉnh khoảng cách giữa các nhóm trong chú thích fill
         margin = list(l = -100, r = 50, b = 100, t = 100, pad = -800, automargin = TRUE)) # Điều chỉnh khoảng cách giữa tên trục y với biểu đồ và di chuyển nó sang phải
gv

#----------------------------------------------------------------------------

#Lọc dữ liệu
data <- read_excel(excel_file_path_chapter_1, sheet = "animal_husbandry")
filtered_data <- data %>%
  filter(Element == "Stocks",
         Item %in% c("Chickens", "Ducks"),
         Area %in% c("Viet Nam", "Thailand", "Philippines", "Malaysia"),
         Year >= 2010 & Year <= 2022)

# Thay đổi giá trị fill cho các loại thịt
filtered_data$Item <- ifelse(filtered_data$Item == "Chickens", "Gà", "Vịt")

# Vẽ biểu đồ cột chồng
slgv <- ggplot(filtered_data, aes(x = Year, y = Value, fill = Item)) +
  geom_col(position = "stack") +
  facet_wrap(~ Area, scales = "free_y") +
  labs(title = "Số lượng đàn gà và vịt",
       x = "Năm",
       y = "Nghìn con",
       fill = "Loại đàn") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("Gà" = "#377EB8", "Vịt" = "#DB4D33"))

# Chuyển đổi sang biểu đồ tương tác và điều chỉnh kích thước
slgv <- ggplotly(slgv, width = 550, height = 400)

# In ra biểu đồ tương tác
slgv


#----------------------------------------------------------------------
#Lọc dữ liệu
data <- read_excel(excel_file_path_chapter_1, sheet = "animal_husbandry")
filtered_data_beef <- data %>%
  filter(Element == "Production",
         Item == "Beef and Buffalo Meat, primary",
         Area %in% c("South-eastern Asia", "Asia"),
         Year >= 2010 & Year <= 2022)

# Tính tỷ lệ phần trăm cho thịt bò và thịt trâu chính
percentage_beef <- filtered_data_beef %>%
  group_by(Year) %>%
  mutate(Percentage_Beef = (Value / sum(Value)) * 100) %>%
  filter(Area == "South-eastern Asia")

# Lọc dữ liệu cho gia cầm
filtered_data_poultry <- data %>%
  filter(Element == "Production",
         Item == "Meat, Poultry",
         Area %in% c("South-eastern Asia", "Asia"),
         Year >= 2010 & Year <= 2022)

# Tính tỷ lệ phần trăm cho gia cầm
percentage_poultry <- filtered_data_poultry %>%
  group_by(Year) %>%
  mutate(Percentage_Poultry = (Value / sum(Value)) * 100) %>%
  filter(Area == "South-eastern Asia")

# Kết hợp dữ liệu về thịt bò và thịt trâu chính với dữ liệu về gia cầm
combined_data <- inner_join(percentage_beef, percentage_poultry, by = "Year")

# Vẽ biểu đồ đường và thêm số liệu
tlpt <- ggplot(combined_data, aes(x = Year)) +
  geom_line(aes(y = Percentage_Beef, color = "Thịt bò và thịt trâu chính"), size = 0.7) +
  geom_line(aes(y = Percentage_Poultry, color = "Gia cầm"), size = 0.7) +
  geom_point(aes(y = Percentage_Beef, color = "Thịt bò và thịt trâu chính"), size = 2) +
  geom_point(aes(y = Percentage_Poultry, color = "Gia cầm"), size = 2)+
  labs(title = "Tỷ lệ sản lượng thịt bò và thịt trâu chính, gia cầm của Đông Nam Á so với Châu Á",
       x = "Năm",
       y = "%") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank()) +
  scale_color_manual(name = "Loại sản phẩm",
                     values = c("Thịt bò và thịt trâu chính" = "#377EB8", "Gia cầm" = "#DB4D33"),
                     labels = c("Thịt bò và thịt trâu chính", "Gia cầm")) +
  scale_x_continuous(breaks = seq(2010, 2022, by = 3)) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(legend.position = "top")+ 
  theme(axis.text.x = element_text(angle = 360, hjust = 1, size = 9.5),
        plot.title = element_text(size = 10, face = "bold")  # Điều chỉnh kích thước và in đậm tiêu đề
  )

# Chuyển đổi sang biểu đồ tương tác và thiết lập kích thước trực tiếp
tlpt <- ggplotly(tlpt, width = 600, height = 400)

# Thêm phần chú thích fill ở dưới
tlpt <- tlpt %>% 
  layout(hoverinfo = "none", legend = list(orientation = "h", 
                                           x = 0.1, y = -0.2, 
                                           bgcolor = "white", 
                                           bordercolor = "black", 
                                           borderwidth = 1,
                                           xanchor = "left", 
                                           traceorder = "normal"))

# Hiển thị biểu đồ tương tác
tlpt




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#CHAPTER 2
# Lấy dữ liệu từ file excel
excel_file_path_chapter_2 <- "CHAPTER2.xlsx"
all_sheet_names_chapter_2 <- excel_sheets(excel_file_path_chapter_2)
print(all_sheet_names_chapter_2)

thuysan_data <- read_excel(excel_file_path_chapter_2, sheet = "thuysan")
channuoi_data <- read_excel(excel_file_path_chapter_2, sheet = "channuoi")
raucu_data <- read_excel(excel_file_path_chapter_2, sheet = "raucu")
lamsan_data <- read_excel(excel_file_path_chapter_2, sheet = "lamsan")
hatdieu_data <- read_excel(excel_file_path_chapter_2, sheet = "hatdieu")
caphe_data <- read_excel(excel_file_path_chapter_2, sheet = "caphe")
gao_data <- read_excel(excel_file_path_chapter_2, sheet = "gao")
gaochaua_data <- read_excel(excel_file_path_chapter_2, sheet = "gao chau a") 

#-------------------------------------------------------------------------
#BIỂU ĐỒ XUẤT KHẨU GẠO CỦA TỪNG KHU VỰC CHÂU Á
# Lọc dữ liệu cho từng khu vực

eastern_asia_export <- subset(gaochaua_data, 
                              `Country or Area` == "Eastern Asia" & Flow == "Export")
southeastern_asia_export <- subset(gaochaua_data, 
                                   `Country or Area` == "South-eastern Asia" & Flow == "Export")
southern_asia_export <- subset(gaochaua_data, 
                               `Country or Area` == "Southern Asia" & Flow == "Export")

# Vẽ biểu đồ cho từng khu vực
plot_ly(eastern_asia_export, x = ~Year, width = 400, height = 500) %>%
  add_bars(y = ~`Weight (tons)`, name = 'Tấn', marker = list(color = '#377EB8')) %>%
  add_lines(y = ~`Trade (US)`, name = 'USD', yaxis = 'y2', line = list(color = '#F57F61')) %>%
  layout(title = list(text = "<b>Số lượng gạo xuất khẩu ở Đông Á</b>", y = 0.98),
         xaxis = list(title = 'Năm', dtick = 2),
         yaxis = list(title = 'Tấn', side = 'right'),  # Chuyển trục y sang phải
         yaxis2 = list(title = 'USD', side = 'left', overlaying = 'y', position = 0),  # Đặt vị trí của trục y2
         legend = list(orientation = 'h', x = 0.15, y = -0.2, bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "left", 
                       traceorder = "normal"))  # Điều chỉnh vị trí chú thích

DNA<-plot_ly(southeastern_asia_export, x = ~Year, width = 550, height = 350) %>%
  add_bars(y = ~`Weight (tons)`, name = 'Tấn', marker = list(color = '#377EB8')) %>%
  add_lines(y = ~`Trade (US)`, name = 'USD', yaxis = 'y2', line = list(color = '#F57F61')) %>%
  layout(title = list(text = "<b>Số lượng gạo xuất khẩu ở Đông Nam Á</b>", y = 0.98),
         xaxis = list(title = 'Năm', dtick = 2),
         yaxis = list(title = 'Tấn', side = 'right'),  # Chuyển trục y sang phải
         yaxis2 = list(title = 'USD', side = 'left', overlaying = 'y', position = 0),  # Đặt vị trí của trục y2
         legend = list(orientation = 'h', x = 0.15, y = -0.2, bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "left", 
                       traceorder = "normal"))  # Điều chỉnh vị trí chú thích
DNA
plot_ly(southern_asia_export, x = ~Year) %>%
  add_bars(y = ~`Weight (tons)`, name = 'Tấn', marker = list(color = '#377EB8')) %>%
  add_lines(y = ~`Trade (US)`, name = 'USD', yaxis = 'y2', line = list(color = '#F57F61')) %>%
  layout(title = list(text = "<b>Số lượng gạo xuất khẩu ở Nam Á</b>", y = 0.98),
         xaxis = list(title = 'Năm', dtick = 2),
         yaxis = list(title = 'Tấn', side = 'right'),  # Chuyển trục y sang phải
         yaxis2 = list(title = 'USD', side = 'left', overlaying = 'y', position = 0),  # Đặt vị trí của trục y2
         legend = list(orientation = 'h', x = 0.15, y = -0.15, bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "left", 
                       traceorder = "normal"))  # Điều chỉnh vị trí chú thích

#----------------------------------------------------------------------------------------------
#CÀ PHÊ
# Lọc dữ liệu theo các mặt hàng cà phê và ASEAN
export_import_coffee<- caphe_data[caphe_data$"Country or Area" == "ASEAN" & 
                                    caphe_data$Commodity %in% c("Coffee; not roasted or decaffeinated", 
                                                                "Coffee; roasted, not decaffeinated", 
                                                                "Coffee; roasted, decaffeinated",
                                                                "Coffee; decaffeinated, not roasted",
                                                                "Coffee; husks and skins, coffee substitutes containing coffee in any proportion"), ]

# Tính số lượng (kg) xuất khẩu và nhập khẩu của mỗi năm
total_quatity_by_year_coffee<- aggregate(`Quantity` ~ Year + Flow, data = export_import_coffee, FUN = sum)


# Biểu đồ cột chồng
cf <- plot_ly(total_quatity_by_year_coffee, x = ~Year, y = ~`Quantity`, color = ~Flow,
              colors = c("Export" = "#F5AF72", "Import" = "#377EB8"),  
              type = 'bar', 
              marker = list(line = list(width = 0))) %>%
  layout(title = list(text = "<b>Tổng lượng xuất và nhập khẩu cà phê của ASEAN</b>", y = 0.99),  # In đậm title
         xaxis = list(title = "Năm"),
         yaxis = list(title = "Kg"),
         barmode = 'stack') %>%
  config(displayModeBar = TRUE)
# Chuyển đổi sang biểu đồ tương tác
cf <- ggplotly(cf, width = 500, height = 400)
# Thêm phần chú thích fill ở dưới
cf <- cf %>% 
  layout(hoverinfo = "none",legend = list(orientation = "h", 
                                          x = 0.3, y = -0.15, 
                                          bgcolor = "white", 
                                          bordercolor = "black", 
                                          borderwidth = 1,
                                          xanchor = "left", 
                                          traceorder = "normal"),
         panel.grid.major.y = list(size = 0.5, color = "gray", linetype = "dashed"),
         panel.grid.minor = list(display = "none"))

cf

#-----------------------------------------------------------------------------------------------
#Vẽ biểu đồ kim ngạch xuất khẩu cà phê các quốc gia ASEAN
export_caphe <- caphe_data[caphe_data$Flow == "Export", ]
# Lọc dòng có các loại mặt hàng cà phê được xuất khẩu từ data
coffee_commodities <- c( "Coffee; not roasted or decaffeinated", 
                         "Coffee; roasted, not decaffeinated", 
                         "Coffee; roasted, decaffeinated",
                         "Coffee; decaffeinated, not roasted",
                         "Coffee; husks and skins, coffee substitutes containing coffee in any proportion")
export_coffee_data <- export_caphe[export_caphe$Commodity %in% coffee_commodities, ]

# Lọc tên 10 quốc gia ASEAN trong data
target_countries <- c("Brunei Darussalam", "Cambodia", 
                      "Indonesia", "Lao People's Dem. Rep.", 
                      "Malaysia", "Myanmar", "Philippines",
                      "Singapore", "Thailand", "Viet Nam")
top_export_caphe <- export_coffee_data[export_coffee_data$"Country or Area" %in% target_countries, ]

# Tính tổng kim ngạch xuất khẩu của mỗi năm cho mỗi quốc gia
total_quantity_by_year <- aggregate(`Trade (USD)`~ Year + `Country or Area`, data = top_export_caphe, FUN = sum)

# Lấy tổng kim ngạch xuất khẩu của các quốc gia bạn quan tâm
total_quantity_by_country <- aggregate(`Trade (USD)` ~ Year + `Country or Area`, data = total_quantity_by_year, FUN = sum)

# Chọn các nước thành viên ASEAN
total_quantity_by_country <- total_quantity_by_country[total_quantity_by_country$"Country or Area" %in% target_countries, ]
#Sử dụng bảng màu bạn đã chỉ định
custom_colors <- c("#4daf4a", "#F5AF72", "#DB4D33", "#984ea3", "orange",
                   "#8dd3c7", "#a65628", "#f781bf", "yellow", "#377eb8")

# Vẽ biểu đồ miền thể hiện kim ngạch xuất khẩu cà phê các quốc giá ASEAN qua các năm
# Biểu đồ miền thể hiện kim ngạch xuất khẩu cà phê các quốc gia ASEAN qua các năm
coffee_plot <- ggplot(total_quantity_by_country, aes(x = Year, y = `Trade (USD)`, fill = `Country or Area`)) +
  geom_area(color = "white", size = 0.5, alpha = 0.9) +  # Tô màu miền và chỉnh đường cong
  labs(title = "<b>Tổng kim ngạch xuất khẩu cà phê các nước thành viên ASEAN</b>",
       x = "Năm",
       y = "USD",
       fill = "Quốc gia") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(face = "bold", size = 10, hjust = 0))+  # In đậm title và canh giữa
  scale_x_continuous(breaks = seq(2000, 2020, by = 2))  # Chia trục x thành số chẵn và cách nhau 2 năm một lần
# Chuyển đổi sang biểu đồ tương tác
coffee_plot <- ggplotly(coffee_plot, width = 600, height = 400)
coffee_plot

#-----------------------------------------------------------------------
#HAT DIEU
#Biều đồ tổng sản lượng xuất nhập khẩu hạt điều các nước ASEAN
export_import_cashew_nuts <- hatdieu_data[hatdieu_data$"Country or Area" == "ASEAN" & 
                                            hatdieu_data$Commodity %in% c("Nuts, edible; cashew nuts, fresh or dried, in shell",
                                                                          "Nuts, edible; cashew nuts, fresh or dried, shelled"), ]

# Tính số lượnglượng xuất khẩu và nhập khẩu của mỗi năm
total_quantity_by_year_cashew_nuts<- aggregate(`Quantity` ~ Year + Flow, data = export_import_cashew_nuts, FUN = sum)

#Vẽ biểu đồ
plot_cashew_nuts <- ggplot(total_quantity_by_year_cashew_nuts, aes(x = Year, y = `Quantity`, fill = Flow)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  labs(title = "<b>Tổng lượng xuất và nhập khẩu hạt điều của ASEAN</b>",
       x = "Năm",
       y = "Kg",
       fill = "Dòng") +
  scale_fill_manual(values = c("Export" = "#DB4D33", "Import" = "#377eb8")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 10, hjust = 0)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 2))  # Chia trục x thành số chẵn và cách nhau 2 năm một lần

# Chuyển đổi sang biểu đồ tương tác
plot_cashew_nuts <- ggplotly(plot_cashew_nuts, width = 600, height = 400)
plot_cashew_nuts
#-----------------------------------------------------------------------
#Kim ngạch xuất khẩu hạt điều của từng nước thành viên
#Lọc dữ liệu khu vực ASEAN
export_cashew_nuts <- hatdieu_data[hatdieu_data$Flow == "Export", ]

# Lọc dòng có các loại mặt hàng hạt điều được xuất khẩu từ data
cashew_nuts_commodities <- c("Nuts, edible; cashew nuts, fresh or dried, in shell",
                             "Nuts, edible; cashew nuts, fresh or dried, shelled")
export_cashew_nuts_data <- export_cashew_nuts[export_cashew_nuts$Commodity %in% cashew_nuts_commodities, ]

# Lọc tên 10 quốc gia ASEAN trong data
target_countries <- c("Brunei Darussalam", "Cambodia", 
                      "Indonesia", "Lao People's Dem. Rep.", 
                      "Malaysia", "Myanmar", "Philippines",
                      "Singapore", "Thailand", "Viet Nam")
top_export_cashew_nuts <- export_cashew_nuts_data[export_cashew_nuts_data$"Country or Area" %in% target_countries, ]

# Tính tổng kim ngạch xuất khẩu của mỗi năm cho mỗi quốc gia
total_quantity_cashew_nuts_by_year <- aggregate(`Trade (USD)` ~ Year + `Country or Area`, 
                                                data = top_export_cashew_nuts, FUN = sum)

# Lấy tổng kim ngạch xuất khẩu của các quốc gia bạn quan tâm
total_quantity_cashew_nuts_by_country <- aggregate(`Trade (USD)` ~ Year + `Country or Area`, 
                                                   data = total_quantity_cashew_nuts_by_year, FUN = sum)

# Chọn các nước thành viên ASEAN
total_quantity_cashew_nuts_by_country <- total_quantity_cashew_nuts_by_country[
  total_quantity_cashew_nuts_by_country$"Country or Area" %in% target_countries, ]

# Vẽ biểu đồ miền thể hiện kim ngạch xuất khẩu hạt điều của các quốc gia ASEAN qua các năm
# Định nghĩa bảng màu sắc tùy chỉnh cho từng quốc gia
custom_colors <- c("#D8B370", "#524F91", "#DA5559", "#305032", "#F5AF72","#DE5C1F","#6B8567","#5B68A9","#F57F61","#E5842D")  # Thay đổi các mã màu theo ý muốn

# Vẽ biểu đồ miền thể hiện kim ngạch xuất khẩu hạt điều của các quốc gia ASEAN qua các năm
cashew_plot <- ggplot(total_quantity_cashew_nuts_by_country, aes(x = Year, y = `Trade (USD)`, fill = `Country or Area`)) +
  geom_area(color = "white", size = 0.5, alpha = 0.9) +  # Tô màu miền và chỉnh đường cong
  labs(title = "Tổng kim ngạch xuất khẩu hạt điều các nước thành viên ASEAN",
       x = "Năm",
       y = "USD",
       fill = "Quốc gia") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 10, hjust = 0)) +
  scale_fill_manual(values = custom_colors) +  # Sử dụng bảng màu sắc tùy chỉnh
  scale_x_continuous(breaks = seq(2000, 2020, by = 2))  # Chia trục x thành số chẵn và cách nhau 2 năm một lần

# Chuyển đổi sang biểu đồ tương tác
cashew_plot <- ggplotly(cashew_plot, width = 650, height = 400)
cashew_plot


#-----------------------------------------------------------------------
#BIỂU ĐỒ XUẤT NHẬP KHẨU CỦA RAU CỦ ASEAN
import_raucu <- raucu_data %>%
  filter(`Country or Area` == "ASEAN", Flow == "Import" & !is.na(`Weight (kg)`)) %>%
  group_by(Year) %>%
  summarise(Total_Weight = sum(`Weight (kg)`), 
            Total_trade = sum(`Trade (USD)`))

export_raucu <- raucu_data %>%
  filter(`Country or Area` == "ASEAN", Flow == "Export" & !is.na(`Weight (kg)`)) %>%
  group_by(Year) %>%
  summarise(Total_Weight = sum(`Weight (kg)`), 
            Total_trade = sum(`Trade (USD)`)) 

stacked_data <- rbind(
  transform(import_raucu, Type = "Import"),
  transform(export_raucu, Type = "Export")
)

raucu_plot <- plot_ly(stacked_data, x = ~Year, y = ~Total_Weight, color = ~Type, type = "bar", colors = c("Import" = "#377eb8", "Export" = "#DB4D33")) %>%
  layout(title = list(text = "<b>Tổng sản lượng xuất nhập khẩu rau của của ASEAN</b>"),
         xaxis = list(title = "Năm", showgrid = FALSE),  # Ẩn lưới trục x
         yaxis = list(title = "Kg", side = "left", showgrid = FALSE),  # Ẩn lưới trục y
         barmode = "stack",
         plot_bgcolor = "rgba(0,0,0,0)",  # Ẩn đường kẻ sọc đằng sau biểu đồ
         legend = list(x = 0, y = 1, bgcolor = "rgba(255, 255, 255, 0.5)"))

# Chuyển đổi sang biểu đồ tương tác
raucu_plot <- ggplotly(raucu_plot, width = 550, height = 400)

# Thêm phần chú thích fill ở dưới
raucu_plot <- raucu_plot %>% 
  layout(hoverinfo = "none",
         legend = list(orientation = "h", 
                       x = 0.5, y = -0.15, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "center",  
                       traceorder = "normal",  
                       itemsizing = "constant",  
                       itemwidth = 50 
         )
  )

raucu_plot


#-----------------------------------------------------------------------
#LÂM NGHIỆP
#Vẽ biều đồ miền giá trị xuất khẩu lâm sản các nước ASEAN
#Lọc dữ liệu 
export_lamsan <- lamsan_data %>%
  filter(Element == "Export Value")
# Tạo biểu đồ tương tác từ biểu đồ ggplot2
chart_1 <- ggplot(export_lamsan, aes(x = Year, y = Value, fill = `Country or Area`)) +
  geom_area() +
  labs(title = "Tổng kim ngạch xuất khẩu lâm sản theo năm của các quốc gia ASEAN",
       x = "Năm",
       y = "1000 USD") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 10, hjust = 0))+
  scale_x_continuous(breaks = seq(2000, 2020, by = 2))  # Chia trục x thành số chẵn và cách nhau 2 năm một lần

# Chuyển đổi thành biểu đồ tương tác
chart_1<- ggplotly(chart_1, width = 730, height = 400)
chart_1

#-----------------------------------------------------------------------
#Tổng kim ngạch xuất khẩu gỗ 2010
export_2010 <- export_lamsan %>%
  filter(Year == 2010)

# Vẽ biểu đồ tổng kim nghạch xuất khẩu lâm sản các nước thành viên ASEAN 2010
ls <- plot_ly(export_2010, labels = ~`Country or Area`, values = ~Value, type = 'pie',
              textposition = 'inside', textinfo = 'percent+label') %>%
  layout(title = list(text = "<b>Tổng kim ngạch xuất khẩu lâm sản ASEAN 2010</b>"),  # In đậm tên của title
         scene = list(
           camera = list(
             eye = list(x = 1.5, y = 1.5, z = 1.5)
           )
         ),
         colorway = custom_colors)


# Chuyển đổi sang biểu đồ tương tác và chỉnh kích thước
interactive_ls <- ggplotly(ls)

# Hiển thị biểu đồ tương tác
interactive_ls
#-----------------------------------------------------------------------
#Tổng kim ngạch xuất khẩu gỗ 2022
export_2022 <- export_lamsan %>%
  filter(Year == 2022)
# Vẽ biểu đồ tổng kim nghạch xuất khẩu lâm sản các nước thành viên ASEAN 2022
ls_2022 <- plot_ly(export_2022, labels = ~`Country or Area`, values = ~Value, type = 'pie',
                   textposition = 'inside', textinfo = 'percent+label') %>%
  layout(title = list(text = "<b>Tổng kim ngạch xuất khẩu lâm sản ASEAN 2022</b>"),  # In đậm tên của title
         scene = list(
           camera = list(
             eye = list(x = 1.5, y = 1.5, z = 1.5)
           )
         ),
         colorway = custom_colors)

# Chuyển đổi sang biểu đồ tương tác và chỉnh kích thước
interactive_ls_2022 <- ggplotly(ls_2022)
# Hiển thị biểu đồ tương tác
interactive_ls_2022
#-----------------------------------------------------------------------
# Tổng hợp dữ liệu xuất khẩu và nhập khẩu theo từng năm của ASEAN
export_import_summary <- lamsan_data %>%
  group_by(Year, Element) %>%
  summarise(Total_Value = sum(Value))

# Tạo biểu đồ cột chồng
chart_2 <- ggplot(export_import_summary, aes(x = Year, y = Total_Value, fill = Element)) +
  geom_bar(stat = "identity") +
  labs(title = "Ngành thương mại lâm nghiệp ASEAN (2010-2022)",
       x = "Năm",
       y = "1000 USD",
       fill = "Loại") +  
  scale_fill_manual(values = c("Export Value" = "#DB4D33", "Import Value" = "#377eb8"),
                    labels = c("Xuất khẩu", "Nhập khẩu")) +  
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))

# Chuyển đổi sang biểu đồ tương tác
chart_2 <- ggplotly(chart_2, width = 600, height = 400)

# Thêm phần chú thích fill ở dưới và chuyển vị trí của ô chú thích sang bên dưới
chart_2 <- chart_2 %>% 
  layout(hoverinfo = "none",
         legend = list(orientation = "h", 
                       x = 0.5, y = -0.2, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "center",  
                       traceorder = "normal",  
                       itemsizing = "constant",  
                       itemwidth = 50  
         )) 

# Hiển thị biểu đồ tương tác
chart_2
#------------------------------------------------------------------------------------------
#CHĂN NUÔI
# Lọc dữ liệu theo các mặt hàng thịt heo và ASEAN
export_import_pork <- channuoi_data[channuoi_data$"Country or Area" == "ASEAN" & 
                                      channuoi_data$Commodity %in% c("Meat; of swine, hams, shoulders and cuts thereof, with bone in, fresh or chilled",
                                                                     "Meat; of swine, n.e.c. in item no. 0203.1, fresh or chilled",
                                                                     "Meat; of swine, carcasses and half-carcasses, frozen",
                                                                     "Meat; of swine, hams, shoulders and cuts thereof, with bone in, frozen",
                                                                     "Meat; of swine, n.e.c. in item no. 0203.2, frozen"), ]

# Tính tổng kim ngạch xuất khẩu và nhập khẩu của mỗi năm
total_quantity_by_year_pork<- aggregate(`Quantity` ~ Year + Flow, data = export_import_pork, FUN = sum)

# Biểu đồ cột chồng
chart_pork <- plot_ly(total_quantity_by_year_pork, x = ~Year, y = ~`Quantity`, color = ~Flow,
                      colors = c("Export" = "#DB4D33", "Import" = "#377EB8"),  
                      type = 'bar', 
                      marker = list(line = list(width = 0))) %>%
  layout(title = "Tổng lượng xuất và nhập khẩu thịt heo của ASEAN",
         xaxis = list(title = "Năm", showgrid = FALSE),
         yaxis = list(title = "Kg", showgrid = FALSE),
         barmode = 'stack') %>%
  config(displayModeBar = TRUE)

# Chuyển đổi sang biểu đồ tương tác
chart_pork <- plot_ly(total_quantity_by_year_pork, x = ~Year, y = ~`Quantity`, color = ~Flow,
                      colors = c("Export" = "#DB4D33", "Import" = "#377EB8"),  
                      type = 'bar', 
                      marker = list(line = list(width = 0))) %>%
  layout(title = "Tổng lượng xuất và nhập khẩu thịt heo của ASEAN",
         xaxis = list(title = "Năm", showgrid = FALSE),  # Ẩn lưới trục x
         yaxis = list(title = "Kg", showgrid = FALSE),  # Ẩn lưới trục y
         barmode = 'stack') %>%
  config(displayModeBar = TRUE)

# Chuyển đổi sang biểu đồ tương tác
chart_pork <- ggplotly(chart_pork, width = 600, height = 400) %>%
  layout(hoverinfo = "none",
         legend = list(orientation = "h", 
                       x = 0.5, y = -0.15, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "center",  
                       traceorder = "normal",  
                       itemsizing = "constant",  
                       itemwidth = 50  
         ),
         margin = list(l = 50, r = 50, b = 50, t = 50),  # Điều chỉnh lề
         font = list(family = "Arial"),  # Đặt font chữ
         title = list(text = "<b>Tổng lượng xuất và nhập khẩu thịt heo của ASEAN</b>"),  # Đặt font cho title
         showlegend = TRUE # Hiển thị chú thích
  )
# Hiển thị biểu đồ tương tác
chart_pork

#------------------------------------------------------------------------------------------
#Biểu đồ Kim ngạcH xuất khẩu thịt heo các nước ASEAN
export_pork <- channuoi_data[channuoi_data$Flow == "Export", ]
# Lọc dòng có các loại mặt hàng thịt heo xuất khẩu
pork_commodities <- c("Meat; of swine, carcasses and half-carcasses, fresh or chilled", 
                      "Meat; of swine, hams, shoulders and cuts thereof, with bone in, fresh or chilled", 
                      "Meat; of swine, n.e.c. in item no. 0203.1, fresh or chilled", 
                      "Meat; of swine, carcasses and half-carcasses, frozen", 
                      "Meat; of swine, hams, shoulders and cuts thereof, with bone in, frozen", 
                      "Meat; of swine, n.e.c. in item no. 0203.2, frozen" )
export_pork_data <- export_pork[export_pork$Commodity %in% pork_commodities, ]

top_export_pork <- export_pork_data[export_pork_data$"Country or Area" %in% target_countries, ]

# Tính tổng kim ngạch xuất khẩu của mỗi năm cho mỗi quốc gia
total_trade_by_year_pork <- aggregate(`Trade (USD)` ~ Year + `Country or Area`, data = top_export_pork, FUN = sum)

# Lấy tổng kim ngạch xuất khẩu của các quốc gia bạn quan tâm
total_trade_by_country_pork <- aggregate(`Trade (USD)` ~ Year + `Country or Area`, data = total_trade_by_year_pork, FUN = sum)

# Chọn các nước bạn quan tâm
total_trade_by_country_pork <- total_trade_by_country_pork[total_trade_by_country_pork$"Country or Area" %in% target_countries, ]

# Biểu đồ đường với đường cong
chart_export_pork <- ggplot(total_trade_by_country_pork, aes(x = Year, y = `Trade (USD)`, color = `Country or Area`)) +
  geom_line(size = 0.8) +
  labs(title = "Tổng kim ngạch xuất khẩu thịt heo các nước ASEAN",
       x = "Năm",
       y = "USD") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "white", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))


# Chuyển đổi sang biểu đồ tương tác
chart_export_pork <- ggplotly(chart_export_pork, width = 700, height = 400)
# Hiển thị biểu đồ tương tác
chart_export_pork

#----------------------------------------------------------------------------------------
##GIACAM
# Lọc dữ liệu theo các mặt gia cầm và ASEAN
export_import_poultry <- channuoi_data[channuoi_data$"Country or Area" == "ASEAN" & 
                                         channuoi_data$Commodity %in% c("Meat and edible offal; of fowls of the species Gallus domesticus, not cut in pieces, fresh or chilled", 
                                                                        "Meat and edible offal; of fowls of the species Gallus domesticus, not cut in pieces, frozen",
                                                                        "Meat and edible offal; of fowls of the species Gallus domesticus, cuts and offal, fresh or chilled",
                                                                        "Meat and edible offal; of fowls of the species Gallus domesticus, cuts and offal, frozen"), ]

export_import_poultry_subset <- subset(export_import_poultry, Year <= 2021)

total_quatity_by_year_poultry <- aggregate(`Quantity` ~ Year + Flow, data = export_import_poultry_subset, FUN = sum)

chart_poultry <- ggplot(total_quatity_by_year_poultry, aes(x = Year, y = Quantity, fill = Flow)) +
  geom_bar(stat = "identity") +
  labs(title = "Tổng lượng xuất và nhập khẩu gia cầm của ASEAN",
       x = "Năm",
       y = "Kg",
       fill = "Dòng") +
  scale_fill_manual(values = c("Export" = "#F5AF72", "Import" = "#514E90")) +  # Đặt màu cho cột xuất khẩu và nhập khẩu
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0)) +
  scale_x_continuous(breaks = seq(min(total_quatity_by_year_poultry$Year), max(total_quatity_by_year_poultry$Year), by = 1))  # Chia trục x thành các năm

# Chuyển đổi sang biểu đồ tương tác
chart_poultry <- ggplotly(chart_poultry, width = 700, height = 400)
# Thêm phần chú thích fill ở dưới và chuyển vị trí của ô chú thích sang bên dưới
chart_poultry

#------------------------------------------------------------------------------

export_poultry <- channuoi_data[channuoi_data$Flow == "Export", ]

# Lọc dòng có các loại mặt hàng gia cầm xuất khẩu 
poultry_commodities <- c("Meat and edible offal; of fowls of the species Gallus domesticus, not cut in pieces, fresh or chilled", 
                         "Meat and edible offal; of fowls of the species Gallus domesticus, not cut in pieces, frozen",
                         "Meat and edible offal; of fowls of the species Gallus domesticus, cuts and offal, fresh or chilled",
                         "Meat and edible offal; of fowls of the species Gallus domesticus, cuts and offal, frozen")
export_poultry_data <- export_poultry[export_poultry$Commodity %in% poultry_commodities, ]

top_export_poultry <- export_poultry_data[export_poultry_data$"Country or Area" %in% target_countries, ]

# Tính tổng kim ngạch xuất khẩu của mỗi năm cho mỗi quốc gia
total_trade_by_year_poultry <- aggregate(`Trade (USD)` ~ Year + `Country or Area`, data = top_export_poultry, FUN = sum)

# Lấy tổng kim ngạch xuất khẩu của các quốc gia bạn quan tâm
total_trade_by_country_poultry <- aggregate(`Trade (USD)` ~ Year + `Country or Area`, data = total_trade_by_year_poultry, FUN = sum)

# Chọn các nước bạn quan tâm
total_trade_by_country_poultry <- total_trade_by_country_poultry[total_trade_by_country_poultry$"Country or Area" %in% target_countries, ]

# Biểu đồ đường với đường cong
# Biểu đồ đường với đường cong
chart_poultry2 <- ggplot(total_trade_by_country_poultry, aes(x = Year, y = `Trade (USD)`, color = `Country or Area`)) +
  geom_line(size = 0.5) +
  labs(title = "Tổng kim nghạch xuất khẩu gia cầm của ASEAN",
       x = "Năm",
       y = "USD") +
  scale_color_manual(values = custom_colors) +  # Đặt màu cho từng quốc gia
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0)) +
  scale_x_continuous(breaks = seq(min(total_trade_by_country_poultry$Year), max(total_trade_by_country_poultry$Year), by = 2))  # Chia trục x thành các năm

# Chuyển đổi sang biểu đồ tương tác
chart_poultry2 <- ggplotly(chart_poultry2, width = 700, height = 400)
# Hiển thị biểu đồ tương tác
chart_poultry2
#---------------------------------------------------------------------------
#THỦY SẢN
# Lọc dữ liệu của từng loại thủy sản
loai_thuysan <- thuysan_data %>%
  mutate(Commodity = sub(";.*", "", Commodity))

import_thuysan <- subset(loai_thuysan, Flow == "Import")
export_thuysan <- subset(loai_thuysan, Flow == "Export")

# Tính tổng sản lượng và phần trăm của từng loại thủy sản nhập khẩu
total_import <- import_thuysan %>%
  group_by(Commodity) %>%
  summarise(`Total_Import (kg)` = sum(`Weight (kg)`, na.rm = TRUE)) %>%
  mutate(Percentage = (`Total_Import (kg)` / sum(`Total_Import (kg)`)) * 100) %>%
  arrange(desc(`Percentage`))
custom_colors <- c("#B31312", "#514E90", "#EEE2DE", "#6B8567", "#EA906C")
# Lấy top 5 loại thủy sản nhập khẩu cao nhất
top_5import <- head(total_import, 5)
Commodity <- sub("\\s*\\(.*", "", top_5import$Commodity)
top_5import <- data.frame(Commodity = Commodity, 
                          `Total_Import (kg)` = top_5import$`Total_Import (kg)`, 
                          Percentage = top_5import$Percentage)

# BIỂU ĐỒ TOP 5 LOẠI THỦY SẢN NHẬP NHẬP KHẨU CỦA NÔNG SẢN
interactive_pie_chart1 <- plot_ly(top_5import, labels = ~Commodity, values = ~Percentage, type = "pie", textinfo = "percent",
                                  hoverinfo = "text", 
                                  text = ~paste0(Commodity, ": ", round(Percentage), "%"),
                                  marker = list(colors = custom_colors),  # Sử dụng custom_colors từ đoạn code dưới
                                  domain = list(x = c(0, 1), y = c(0, 1)),
                                  sort = FALSE) %>%
  layout(title = "<b>Top 5 loại thủy sản được nhập khẩu nhiều nhất ở ASEAN</b>",
         showlegend = TRUE)

# Hiển thị biểu đồ tương tác
interactive_pie_chart1

# Tính tổng sản lượng và phần trăm của từng loại thủy sản xuất khẩu
total_export <- export_thuysan %>%
  group_by(Commodity) %>%
  summarise(`Total_Export (kg)` = sum(`Weight (kg)`, na.rm = TRUE)) %>%
  mutate(Percentage = (`Total_Export (kg)` / sum(`Total_Export (kg)`)) * 100) %>%
  arrange(desc(`Percentage`))
custom_colors <- c("#B31312", "#514E90", "#EEE2DE", "#6B8567", "#EA906C")

# Lấy top 5 loại thủy sản xuất khẩu cao nhất
top_5export <- head(total_export, 5)
Commodity_export <- sub("\\s*\\(.*", "", top_5export$Commodity)
top_5export <- data.frame(Commodity = Commodity_export, 
                          `Total_Export (kg)` = top_5export$`Total_Export (kg)`, 
                          Percentage = top_5export$Percentage)

# BIỂU ĐỒ TOP 5 LOẠI THỦY SẢN XUẤT NHẬP KHẨU CỦA NÔNG SẢN
interactive_pie_chart_export <- plot_ly(top_5export, labels = ~Commodity, values = ~Percentage, type = "pie", textinfo = "percent",
                                        hoverinfo = "text", 
                                        text = ~paste0(Commodity, ": ", round(Percentage), "%"),
                                        marker = list(colors = custom_colors),  # Sử dụng custom_colors từ đoạn code dưới
                                        domain = list(x = c(0, 1), y = c(0, 1)),
                                        sort = FALSE) %>%
  layout(title = "<b>Top 5 loại thủy sản được xuất khẩu nhiều nhất ở ASEAN</b>",
         showlegend = TRUE)
# Hiển thị biểu đồ tương tác
interactive_pie_chart_export



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


#CHAPTER3: CHỈ SỐ AN NINH
# Lấy dữ liệu từ file excel
excel_file_path_chapter_3 <- "CHAPTER3.xlsx"


# Đọc dữ liệu từ file Excel (.xls)
data <- read_excel(excel_file_path_chapter_3, sheet = "Dietary_energy")

# Xóa các cột không cần thiết
data_clean <- data %>%
  select(Area, Year, Value) %>%
  filter(Area %in% c("Vietnam", "Lao", "Cambodia", 
                     "Thailand", "Myanmar", "Malaysia", "Philippines", "Indonesia"))

install.packages("plotly")
library(dplyr)
library(plotly)
# Chọn và lọc dữ liệu cho các quốc gia Đông Nam Á cần phân tích
countries <- c("Viet Nam", "Lao", "Cambodia", 
               "Thailand", "Myanmar", "Malaysia", "Philippines", "Indonesia")

data_clean <- data %>%
  filter(Area %in% countries) %>%
  select(Area, Year, Value)

# Chuyển đổi dữ liệu trong cột Area sang factor để sắp xếp thứ tự trên biểu đồ
data_clean$Area <- factor(data_clean$Area, levels = countries)

# Tạo biểu đồ đường tương tác sử dụng plotly
calo_plot <- plot_ly(data_clean, x = ~Year, y = ~Value, color = ~Area, type = 'scatter', mode = 'lines+markers', colors = custom_colors) %>%
  layout(title = list(text = "<b>Bình quân calo mỗi ngày của Đông Nam A<b>",
                      y = 0.95,  # Đặt tiêu đề ở trên cùng
                      xref = "paper"),
         xaxis = list(title = "Năm", showgrid = TRUE),
         yaxis = list(title = "kcal/cap/day", side = "left", showgrid = TRUE),
         legend = list(x = 0, y = 1, bgcolor = "rgba(255, 255, 255, 0.5)"),
         plot_bgcolor = "rgba(0,0,0,0)") 

calo_plot <- ggplotly(calo_plot, width = 650, height = 500)
# Thêm phần chú thích fill ở dưới
calo_plot <- calo_plot %>% 
  layout(hoverinfo = "none",
         legend = list(orientation = "h", 
                       x = 0.5, y = -0.3, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "center",  
                       traceorder = "normal",  
                       itemsizing = "constant",  
                       itemwidth = 50  
         ))
calo_plot

#-------------------------------------------------------------------------------------------

raw_data <- read_excel(excel_file_path_chapter_3, sheet = "Food Security")
class(raw_data)
colnames(raw_data)
dim(raw_data)

# Define a helper function
empty_as_na <- function(x) {
  if ("factor" %in% class(x)) x <- as.character(x) 
  ifelse(as.character(x) != "", x, NA)
}

# Transform all columns
clean_data <- raw_data %>% mutate(across(everything(), empty_as_na))
#clean_data <- as_data_frame(apply(raw_data, 2, function(x) gsub("^$|^ $", NA, x)))
clean_data <- raw_data %>% filter(Flag == "A" | Flag == "E"| Flag == "X") %>% separate(Year,into = c("Year_From","Year_To"))
clean_data$`Area Code (M49)` <- as.character(clean_data$`Area Code (M49)`)
clean_data$`Item Code` <- factor(clean_data$`Item Code`)
clean_data$`Element Code` <- factor(clean_data$`Element Code`)
clean_data$Year_From <-  as.numeric(clean_data$Year_From)
clean_data$Year_To <- as.numeric(clean_data$Year_To)

datatable(head(clean_data,50))

str(clean_data)

glimpse(clean_data)

#Prevalence and number of undernourished people in the world
#Tidy data for Undernourished
asian_colors <- c("#514E90", "#DE5C1F", "#F5AF72", "#5B68A9", "#DA5559", "#E5842D", 
                  "#6B8567", "#305032", "#D8B370", "#111D59")
Undernourished <- clean_data %>% filter(`Item Code` == 210011 )
# Get unique values of Area.Code..M49. corresponding to Item.Code == 210010
unique_areas <- Undernourished %>% distinct(Area)
# Filter the data again based on these unique values
Undernourished_all <- clean_data %>% filter(`Item Code` == 210011 & Area %in% unique_areas$Area)
Undernourished_all$Value <- as.numeric(Undernourished_all$Value)
# Calculate common tick values for y-axis
common_tick_values <- seq(0, max(Undernourished_all$Value), by = max(Undernourished_all$Value) / 5) 
#Plotly graph for undernourished
a <-plot_ly(data = Undernourished_all, x = ~Year_From, y = ~Value, color = ~as.factor(Area),
            type = 'scatter', mode = 'lines', fill = 'tozeroy',
            hoverinfo = 'text',
            text = ~paste('Triệu người:', Value,
                          '</br></br> Năm:', Year_From,
                          '</br> Quốc gia:', Area),
            colors = asian_colors) %>%
  layout(title = list(text = "<b>Số lượng người suy dinh dưỡng Đông Nam Á<b>",
                      y = 0.95,  # Đặt tiêu đề ở trên cùng
                      xref = "paper"),  # Xác định vị trí của x dựa trên toàn bộ plot
         xaxis = list(title = "Năm", showgrid = TRUE),
         xaxis = list(title = "Năm", showgrid = FALSE),
         yaxis = list(title = "Triệu người", showgrid = TRUE),
         plot_bgcolor = "rgba(0,0,0,0)",
         legend = list(x = 0, y = 1, bgcolor = "rgba(255, 255, 255, 0.5)"),
         showlegend = TRUE)

a <- ggplotly(a, width = 700, height = 400)
a  <- a  %>% 
  layout(hoverinfo = "none",
         legend = list(orientation = "h", 
                       x = 0.5, y = -0.3, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "center",  
                       traceorder = "normal",  
                       itemsizing = "constant",  
                       itemwidth = 50  
         ))
a 

#Tidy data for Prevalence of Undernourishment in the world
PU <- clean_data %>% filter(`Item Code` == 210041 )
# Get unique values of Area.Code..M49. corresponding to Item.Code == 210010
unique_area <- PU %>% distinct(Area)
PU_all <- clean_data %>% filter(`Item Code` == 210041 & Area %in% unique_areas$Area)
# Convert 'Value' column to numeric
PU_all$Value <- as.numeric(PU_all$Value)
# Calculate common tick values for y-axis
common_tick_values <- seq(0, max(PU_all$Value), by = max(PU_all$Value)/100) # Adjust the interval as needed
#Plotly graph for Prevalence of Undernourishment
b <- PU_all %>%
  plot_ly(x = ~Year_From, y = ~Value,color = ~as.factor(Area),type = 'scatter', mode = 'lines',
          hoverinfo = 'text',colors = asian_colors,
          text = ~paste('Pha : ', Value,
                        '</br></br> Year: ', Year_From,'</br> Country: ', Area)) %>%
  layout(title = list(text = "<b>Tỷ lệ người suy dinh dưỡng của các nước Đông Nam Á<b>",
                      y = 0.95,  # Đặt tiêu đề ở trên cùng
                      xref = "paper"),  # Xác định vị trí của x dựa trên toàn bộ plot
         xaxis = list(title = "Năm", showgrid = TRUE),
         xaxis = list(title = "Năm",showgrid = TRUE),
         yaxis = list(title = "%", side = "left", showgrid = TRUE),
         plot_bgcolor = "rgba(0,0,0,0)",
         legend = list(x = 0, y = 1, bgcolor = "rgba(255, 255, 255, 0.5)"))
# Chuyển đổi sang biểu đồ tương tác
b <- ggplotly(b, width = 650, height = 500)
# Thêm phần chú thích fill ở dưới
b <- b %>% 
  layout(hoverinfo = "none",
         legend = list(orientation = "h", 
                       x = 0.5, y = -0.35, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "center",  
                       traceorder = "normal",  
                       itemsizing = "constant",  
                       itemwidth = 50  
         ))
b

# Filter data for years 2014 and 2022 and relevant indicators
filtered_data <- clean_data %>%
  filter(`Item Code` %in% c("210401","210091"),
         Year_From %in% c(2018,2020))

# Pivot data to wide format for easier plotting
pivot_data <- filtered_data %>%
  select(Area, `Item Code`, Value, Year_From) %>%
  pivot_wider(names_from = Year_From, values_from = Value)
# Chuyển dữ liệu từ dạng rộng sang dạng dài
long_data <- pivot_data %>%
  pivot_longer(cols = c("2018", "2020"), names_to = "Year", values_to = "Value")

# Chuyển đổi giá trị của Value sang dạng số, bỏ qua các giá trị không thể chuyển đổi
long_data$Value <- as.numeric(gsub("<", "", long_data$Value))
long_data$Value[is.na(long_data$Value)] <- 0  # Đặt các giá trị không thể chuyển đổi thành 0 hoặc giá trị khác phù hợp

# Tạo biểu đồ c2


c <- ggplot(long_data, aes(x = Year, y = Value, fill = factor(`Item Code`, labels = c("Trung bình", "Nghiêm trọng")))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Năm", y = "Giá trị(%)", fill = "Mức độ", title = "Mức độ mất an ninh lương thực ASEAN") +
  theme(axis.text.x = element_text(angle = 45,vjust = 2,  hjust=2), # Điều chỉnh góc nghiêng và vị trí chữ
        panel.grid.major = element_blank(),  # Loại bỏ lưới chính
        panel.grid.minor = element_blank()) +  # Loại bỏ lưới phụ
  scale_y_continuous(breaks = seq(0, max(long_data$Value), by = 10)) +
  scale_fill_manual(values = c("Trung bình" = "#22559EFF", "Nghiêm trọng" = "#E33131FF")) +
  facet_wrap(~ Area, nrow = 1) # Bố trí các nước trong một hàng

# Chuyển đổi thành biểu đồ tương tác
c <- ggplotly(c, tooltip = "text", width = 750, height = 400)
c




#----
#BIỂN ĐỒ CÒI DƯỚI 5 TUỔI
#Global rates of stunting among children
stunting <-
  clean_data %>% filter(`Item Code` == 21025,Flag == 'X')
# Get unique values of Area.Code..M49. corresponding to Item.Code == 210010
uniquestunning <- stunting %>% distinct(Area)

# Filter the data again based on these unique values
stunning <- clean_data %>% filter(`Item Code` == 21025 & Area %in% uniquestunning$Area)
stunning$Value <- as.numeric(stunning$Value)
# Calculate common tick values for y-axis
common_tick_values <- seq(0, max(stunning$Value), by = max(stunning$Value)/100) # Adjust the interval as needed
#BIỂU ĐỒ TỶ LỆ CÒI CỦA TRẺ EM DƯỚI 5 TUỔI
thin_people <- stunning %>%
  plot_ly(x = ~Year_From, y = ~Value,color = ~as.factor(Area),type = 'scatter', mode = 'lines',
          hoverinfo = 'text',colors = asian_colors,
          text = ~paste('Năm: ', Year_From,
                        '</br></br> Phần trăm: ', Value,'</br> Quốc gia: ', Area)) %>%
  layout(title = list(text = "<b>Tỷ lệ còi của trẻ dưới 5 tuổi<b>",
                      y= 0.95,  # Đặt tiêu đề ở giữa theo trục x
                      xref = "paper"),  # Xác định vị trí của x dựa trên toàn bộ plot         xaxis = list(title = "Năm",showgrid = FALSE),
         yaxis = list(title = "%",showgrid = TRUE),common_tick_values,showlegend = TRUE,
         xaxis = list(title = "Năm", showgrid = FALSE),
         plot_bgcolor = "rgba(0,0,0,0)",
         legend = list(x = 0, y = 1, bgcolor = "rgba(255, 255, 255, 0.5)"))
# Chuyển đổi sang biểu đồ tương tác
thin_people <- ggplotly(thin_people, width = 650, height = 500)
# Thêm phần chú thích fill ở dưới
thin_people <- thin_people %>% 
  layout(hoverinfo = "none",
         legend = list(orientation = "h", 
                       x = 0.5, y = -0.35, 
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "center",  
                       traceorder = "normal",  
                       itemsizing = "constant",  
                       itemwidth = 50  
         ))
thin_people
#BIỂU ĐỒ TOP 5 QUỐC GIA CÓ TRẺ EM CÒI CAO NHẤT
stunting_top10 <- stunning %>% 
  filter(`Area Code (M49)` != 127,Year_From == 2020) %>%
  arrange(desc(Value)) %>% 
  slice(1:5)

plotly_chart <- plot_ly(stunting_top10, labels = ~Area, parents = NA, values = ~Value, type = "treemap",
                        marker = list(colors = c("#F7DC6F", "#2980B9", "#A93226","#305032","#D8B370")),
                        text = ~paste("Quốc gia: ", Area, "<br>Tỷ lệ: ", Value, "%")) %>%
  layout(title = list(text="<b>Top 5 quốc gia có tỷ lệ trẻ em thấp còi cao nhất (2020)<b>",y=0.95, xref = "paper"),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
plotly_chart




#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

#CHAPTER 4: NÔNG NGHIỆP BỀN VỮNG

# Lấy dữ liệu từ file excel
excel_file_path_chapter_4 <- "CHAPTER4.xlsx"
all_sheet_names_chapter_4 <- excel_sheets(excel_file_path_chapter_4)
print(all_sheet_names_chapter_4)
# Import dữ liệu
data <- read_excel(excel_file_path_chapter_4, sheet = "Temperature")
data <- data %>%
  filter(Element == "Temperature change", Months == "Meteorological year") %>%
  mutate(Value = as.numeric(as.character(Value))) %>%
  group_by(`Year Code`) %>%
  summarise(Mean_Temperature = mean(Value, na.rm = TRUE)) %>%
  rename(Year = `Year Code`)



# Data khác
production_data <- read_excel(excel_file_path_chapter_4, sheet = "Value")
countries <- c("Brunei Darussalam", "Cambodia", "Indonesia", "Lao People's Democratic Republic", 
               "Malaysia", "Philippines", "Thailand", "Viet Nam")
items <- c("Oil palm fruit", "Rice", "Coffee, green",
           "Coconuts, in shell",
           "Natural rubber in primary forms")

# Lọc và tính tổng sản lượng sản phẩm theo mỗi mục và mỗi năm
sum_production <- production_data %>%
  filter(Area %in% countries & Item %in% items) %>%
  group_by(Item, `Year Code`) %>%
  summarise(total_production = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percentage_growth = (total_production - lag(total_production)) / lag(total_production) * 100) %>%
  filter(!is.na(percentage_growth)) %>%
  rename(Year = `Year Code`)

# Merge hai bảng sum_production và mean_temp_by_year theo cột "Year"
merged_data <- merge(sum_production, data, by = "Year", all = TRUE) %>%
  filter(Year >= 1992) %>%
  arrange(Year)

# Tính toán giá trị Min và Max của cột percentage_growth
min_percentage_growth <- min(merged_data$percentage_growth)
max_percentage_growth <- max(merged_data$percentage_growth)
#Đặt tên cột mới 
new_names <- c("Dừa","Cà phê","Cao su","Gạo")  # Điền vào tên mới cho từng giá trị
# Sử dụng mutate để thay đổi tên
merged_data <- merged_data %>%
  mutate(Item = case_when(
    Item == "Coconuts, in shell" ~ new_names[1],
    Item == "Coffee, green" ~ new_names[2],
    Item == "Natural rubber in primary forms" ~ new_names[3],
    Item == "Rice" ~ new_names[4],
    # Thêm các điều kiện thay đổi tên mới ở đây cho các giá trị còn lại
    TRUE ~ as.character(Item)  # Giữ nguyên các giá trị không được thay đổi
  ))
temperature_plot <- ggplot(merged_data) +
  geom_bar(aes(x = Year, y = Mean_Temperature - mean(Mean_Temperature), 
               fill = Mean_Temperature - mean(Mean_Temperature) > 0), 
           stat = "identity", position = "stack") +
  geom_smooth(aes(x = Year, y = percentage_growth, color = Item), method = "loess", se = FALSE,size = 0.5) +
  scale_fill_manual(values = c("#002379", "#FF5F00"), guide = FALSE) +  # Đặt màu cho geom_bar và không hiển thị legend
  scale_color_manual(values = c("#1E0342", "#0E46A3", "#9AC8CD", "#8B322C")) +  # Đặt màu cho geom_smooth và không hiển thị legend
  scale_y_continuous(
    name = "°C",
    limits = c(-5, 5),
    breaks = seq(-5, 5, 1),
    sec.axis = sec_axis(~., breaks = seq(-1000, 1000, by = 100), name = "%")
  ) +
  labs(x = "Năm", 
       title = "Sự thay đổi của nhiệt độ và sản lượng") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Đặt legend ở vị trí dưới cùng của biểu đồ
        legend.title = element_blank(),  # Loại bỏ tiêu đề của legend
        guides(fill = FALSE),  # Loại bỏ legend cho fill
        panel.grid.major = element_line(size = 1),  # Đặt kích thước của lưới chính
        panel.grid.minor = element_blank()) 
temperature_plot
#---------
# Đọc dữ liệu từ file CSV
pesticides <- read_excel(excel_file_path_chapter_4, sheet = "Pesticides")


# Danh sách các quốc gia cần xem
countries <- c("Brunei Darussalam", "Cambodia", "Indonesia", "Lao People's Democratic Republic", 
               "Malaysia", "Philippines", "Thailand", "Viet Nam", "Myanmar")

# Lọc các hàng chứa các quốc gia trong danh sách và lưu vào dataframe mới
pesticides_filtered <- pesticides[pesticides$Area %in% countries, ]

custom_colors <- c("#176B87", "#F5AF72", "#DB4D33", "#984ea3", "#377eb8",
                   "#8dd3c7", "#a65628", "#f781bf", "#D8B370")

# Vẽ biểu đồ miền thể hiện tổng lượng thuốc trừ sâu của các quốc gia ASEAN qua các năm
pesticides_plot <- ggplot(pesticides_filtered, aes(x = Year, y = `Value`, fill = `Area`)) +
  geom_area(color = "white", size = 0.5, alpha = 0.8) +  # Tô màu miền và chỉnh đường cong
  labs(title = "<b>Tổng lượng thuốc trừ sâu của các nước ASEAN trên một ha</b>",
       x = "Năm",
       y = "kg/ha",
       fill = "Quốc gia") +
  scale_fill_manual(values = custom_colors) +  # Sử dụng màu sắc tùy chỉnh
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 10, hjust = 0)) +  # In đậm title và canh giữa
  scale_x_continuous(breaks = seq(2000, 2020, by = 2))  # Chia trục x thành số chẵn và cách nhau 2 năm một lần

# Chuyển đổi sang biểu đồ tương tác
pesticides_plot <- ggplotly(pesticides_plot, width = 700, height = 400)
pesticides_plot
#-------------
#Import dữ liệu
grown_agriculture <- read_excel(excel_file_path_chapter_4, sheet = "Development")
# Tính tổng theo mục đích và tạo dataframe mới
total_by_purpose <- grown_agriculture %>%
  group_by(Purpose) %>%
  summarise(Total_Value = sum(Value)) %>%
  mutate(Percentage = Total_Value / sum(Total_Value) * 100)
#Đặt tên cột mới 
new_names <- c("Chính sách môi trường và quản lý hành chính", "Phát triển nông thôn", "Bảo vệ sinh quyển",
               "Đa dạng sinh học", "Nghiên cứu môi trường", "Nâng cao nhận thức", "Bảo tồn sinh học")  # Điền vào tên mới cho từng giá trị
# Sử dụng mutate để thay đổi tên
total_by_purpose <- total_by_purpose %>%
  mutate(Purpose = case_when(
    Purpose == "Environmental policy and administrative management" ~ new_names[1],
    Purpose == "Rural development" ~ new_names[2],
    Purpose == "Biosphere protection" ~ new_names[3],
    Purpose == "Bio-diversity" ~ new_names[4],
    Purpose == "Environmental research" ~ new_names[5],
    Purpose == "Environmental education/ training" ~ new_names[6],
    Purpose == "Site preservation" ~ new_names[7],
    # Thêm các điều kiện thay đổi tên mới ở đây cho các giá trị còn lại
    TRUE ~ as.character(Purpose)  # Giữ nguyên các giá trị không được thay đổi
  ))
# Vẽ biểu đồ tròn
custom_colors <- c("#E55604", "#514E90", "#D8B370", "#26577C", "#B4B4B3", "#0E0E0C", "#E55604")
capital_purpose <- plot_ly(total_by_purpose, labels = ~`Purpose`, values = ~Total_Value, type = 'pie',
                           textposition = 'inside', textinfo = 'percent') %>%
  layout(title = list(text = "   <b>Phân phối vốn theo mục đích</b>", y = 0.9),  # Điều chỉnh vị trí của title
         scene = list(
           camera = list(
             eye = list(x = 1, y = 1, z = 1)
           )
         ),
         marker = list(colors = custom_colors))  # Sử dụng marker để đặt màu sắc từ custom_colors

# Chuyển đổi sang biểu đồ tương tác và chỉnh kích thước
interactive_capital_purpose <- ggplotly(capital_purpose)

# Thay đổi kích thước của biểu đồ
interactive_capital_purpose <- interactive_capital_purpose %>% 
  layout(width = 700, height = 400)

# Thay đổi tên của ô chú thích từ "Country or Area" thành "Quốc gia" và điều chỉnh vị trí của ô chú thích
interactive_capital_purpose <- interactive_capital_purpose %>% 
  layout(hoverinfo = "none",
         legend = list(orientation = "h"))  # Đặt orientation là "h" để hiển thị ô chú thích ngang

interactive_capital_purpose <- interactive_capital_purpose %>% 
  layout(hoverinfo = "none",
         legend = list(orientation = "h",   # Đặt orientation là "h" để hiển thị ô chú thích ngang
                       x = 1.25,              # Đặt x = 1 để chú thích ở bên phải biểu đồ
                       y = 0.7,            # Đặt y ở giữa theo chiều dọc
                       bgcolor = "white", 
                       bordercolor = "black", 
                       borderwidth = 1,
                       xanchor = "left",   # Đặt xanchor là "left" để căn lề trái của ô chú thích
                       traceorder = "normal",  
                       itemsizing = "constant",  
                       itemwidth = 1))
interactive_capital_purpose

#Import Việt Nam
exprot_vietnam <- grown_agriculture <- read_excel(excel_file_path_chapter_4, sheet = "Vietnam")

# Lọc dữ liệu chỉ chứa mục đích "Coffee, decaffeinated or roasted"
coffee_data <- exprot_vietnam %>%
  filter(Item == "Coffee, decaffeinated or roasted")

# Lọc dữ liệu chỉ chứa mục đích trái cây
fruit_data <- exprot_vietnam %>%
  filter(Item %in% c("Other tropical fruits, n.e.c.", "Other tropical fruit, dried"))

# Lọc dữ liệu chỉ chứa mục đích gạo
rice_data <- exprot_vietnam %>%
  filter(Item %in% c("Rice"))

# Lọc dữ liệu chỉ chứa mục đích hạt điều
peper_data <- exprot_vietnam %>%
  filter(Item %in% c("Chillies and peppers, dry (Capsicum spp., Pimenta spp.), raw","Chillies and peppers, green (Capsicum spp. and Pimenta spp.)
"))

# Tính tổng giá trị theo năm cho cà phê
sum_by_year_coffee <- coffee_data %>%
  group_by(Year) %>%
  summarize(total_value = sum(Value)) %>%
  mutate(product = "Cà phê")

# Tính tổng giá trị theo năm cho trái cây
sum_by_year_fruit <- fruit_data %>%
  group_by(Year) %>%
  summarize(total_value = sum(Value)) %>%
  mutate(product = "Trái cây")

# Tính tổng giá trị theo năm cho gạo
sum_by_year_rice <- rice_data %>%
  group_by(Year) %>%
  summarize(total_value = sum(Value)) %>%
  mutate(product = "Gạo")

# Tính tổng giá trị theo năm cho hạt điều
sum_by_year_pepper <- peper_data %>%
  group_by(Year) %>%
  summarize(total_value = sum(Value)) %>%
  mutate(product = "Hạt tiêu")

# Kết hợp hai dataframe lại
combined_data <- bind_rows(sum_by_year_coffee, sum_by_year_fruit,sum_by_year_rice,sum_by_year_pepper)

# Vẽ biểu đồ
p <- ggplot(combined_data, aes(x = Year, y = total_value, color = product, fill = product)) +
  geom_line(size = 1, show.legend = TRUE) +
  geom_point(size = 1.25, show.legend = TRUE) +
  geom_ribbon(data = subset(combined_data, Year >= 2020),
              aes(ymax = total_value, ymin = 0),
              alpha = 0.5, show.legend = FALSE) +
  labs(title = expression("Tổng giá trị xuất khẩu các mặt hàng nông sản của Việt Nam"),
       x = "Năm",
       y = "1000 USD",
       color = "Sản phẩm",
       fill = "Sản phẩm") +
  scale_x_continuous(breaks = seq(2015, max(combined_data$Year), by = 2)) +
  scale_color_manual(values = c("Cà phê" = "#D8B370", "Trái cây" = "#514E90", "Gạo" = "#DA5559", "Hạt tiêu" = "#305032"), guide = FALSE) +
  scale_fill_manual(values = c("Cà phê" = "#DE5C1F", "Trái cây" = "#ADD8E6", "Gạo" = "#D8B370", "Hạt tiêu" = "#FFA500"), guide = FALSE) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "bold"))  # In đậm tiêu đề

# Chỉnh kích thước của biểu đồ
p <- ggplotly(p, width = 700, height = 400)

# Chỉnh layout
p










#----------------------------------------------------------------------------------
#TẠO TRANG WEB
library(shiny)
library(shinydashboard)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .header {
        position: fixed;
        top: 0;
        left: 50%;
        transform: translateX(-50%);
        z-index: 1000;
        background-color: #FC4100;
        width: 100%;
        padding: 10px 0;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        color: #ffffff;
        font-family: 'Montserrat', sans-serif;
      }
      body, html {
        margin: 0;
        padding: 0;
        height: 100%;
        background-color: #FC4100;
      }
      .thumbnail {
        margin: 0;
        padding: 0;
        border: none;
        background-color: #FC4100;
        object-fit: cover;
        width: 100%;
        height: 630px; /* Adjust height if necessary */
      }
      .book-container {
        width: 100%;
        height: calc(100vh - 60px); /* Adjust if necessary */
        overflow: hidden;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .scroll-button {
        position: fixed;
        right: 50%;
        bottom: 20px;
        cursor: pointer;
        padding: 10px;
        background-color: #ffffff;
        color: #FC4100;
        border-radius: 50%;
        border: none;
        font-size: 24px;
        width: 48px;
        height: 48px;
        display: flex;
        align-items: center;
        justify-content: center;
        transform: translateX(50%);
      }
      .scroll-icon {
        color: #FC4100;
      }
      .chapter-box {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        grid-gap: 15px;
        margin-top: 100px; /* Increased margin-top to push down the chapters */
        max-width: 900px;
        margin: 100px auto 20px; /* Reduced bottom margin to bring rows closer */
      }
      .chapter-box-row2 {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        grid-gap: 15px;
        max-width: 600px;
        margin: 20px auto 100px; /* Reduced top margin to bring rows closer */
      }
      .chapter {
        padding: 15px; /* Reduced padding to decrease size */
        font-size: 24px; /* Adjust font size if necessary */
        color: white;
        cursor: pointer;
        height: 0;
        padding-bottom: 90%; /* Adjust aspect ratio for smaller squares */
        position: relative;
        font-family: 'Montserrat', sans-serif;
        font-weight: bold;
      }
      .chapter-text {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
      }
      .chapter0 { background-color: #4A90E2; }
      .chapter1 { background-color: #1025a1; }
      .chapter2 { background-color: #F20505; }
      .chapter3 { background-color: #F28705; }
      .chapter4 { background-color: #025959; }
    ")),
    tags$script(HTML("
      $(document).on('click', '.scroll-button', function() {
        $('html, body').animate({
          scrollTop: $('#next-section').offset().top
        }, 'slow');
      });

      $(document).on('click', '.chapter', function() {
        var chapterId = $(this).attr('id');
        Shiny.setInputValue('selectedChapter', chapterId);
      });
    "))
  ),
  div(class = "header", 
      titlePanel("TOÀN CẢNH NỀN NÔNG NGHIỆP ASEAN")
  ),
  div(class = "book-container",
      img(class = "thumbnail", src = "https://raw.githubusercontent.com/KimOanh18/test/master/BACKROUND%201.png", 
          alt = "Thumbnail", 
          style = "object-fit: cover; width: 100%; height: 630px;")
  ),
  div(id = "top-section"),
  actionButton("scrollBtn", label = "", icon = icon("arrow-down", lib = "glyphicon"), class = "scroll-button scroll-icon"),
  div(id = "next-section", style="height: auto;",
      div(class = "chapter-box",
          div(class = "chapter chapter0", id = "chapter0",
              div(class = "chapter-text", "TỔNG QUAN NỀN NÔNG NGHIỆP ASEAN")),
          div(class = "chapter chapter1", id = "chapter1",
              div(class = "chapter-text", "CHƯƠNG 1: CÂY TRỒNG VÀ CHĂN NUÔI")),
          div(class = "chapter chapter2", id = "chapter2",
              div(class = "chapter-text", "CHƯƠNG 2: THƯƠNG MẠI NÔNG NGHIỆP"))
      ),
      div(class = "chapter-box-row2",
          div(class = "chapter chapter3", id = "chapter3",
              div(class = "chapter-text", "CHƯƠNG 3: AN NINH LƯƠNG THỰC")),
          div(class = "chapter chapter4", id = "chapter4",
              div(class = "chapter-text", "CHƯƠNG 4: PHÁT TRIỂN BỀN VỮNG"))
      )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$selectedChapter, {
    chapterId <- input$selectedChapter
    switch(chapterId,
           chapter0 = {
             showModal(modalDialog(
               title = HTML("<b>TỔNG QUAN NÔNG NGHIỆP ASEAN</b>"),
               tags$img(src = "https://raw.githubusercontent.com/KimOanh18/git/master/BACKGROUND.png", style = "max-width: 100%; height: auto;"),
               size= "l", style = "width: 100%;"
             ))
           },
           chapter1 = {
             showModal(modalDialog(
               title =  HTML("<b>CHƯƠNG 1: CÂY TRỒNG VÀ CHĂN NUÔI</b>"),
               fluidRow(
                 tags$style(HTML("
        .custom-text {
          font-family: 'Roboto', sans-serif;
          font-size: 15px; /* Tăng cỡ chữ */
        }
      ")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'> <b>1. Câu chuyện về cây lúa: Đông Nam Á phải thu hẹp khoảng cách giữa năng suất và tiềm năng có thể khai thác để tiếp tục là vựa lúa lớn.</b>
                 <br>
                 Trong suốt nửa thế kỷ qua, Đông Nam Á đã đạt được sự tiến bộ đáng kể trong việc tăng sản lượng lúa gạo, chủ yếu nhờ nâng cao năng suất và mật độ canh tác. Nhờ đó các hệ thống trồng lúa gạo ở lưu vực sông và đồng bằng của khu vực đã tạo ra một lượng gạo dư thừa ổn định, vừa đáp ứng nhu cầu nội địa vừa đóng góp một phần quan trọng vào nguồn cung lương thực toàn cầu. Đông Nam Á hiện chiếm 26% sản lượng và 40% xuất khẩu gạo trên thế giới, đồng thời là nguồn cung chính cho các thị trường như Châu Phi và Trung Đông. Dự báo về sự tăng trưởng 30% trong nhu cầu gạo toàn cầu vào năm 2050 đặt ra một thách thức mới nhưng cũng tiếp tục đóng vai trò quan trọng trong cung cấp gạo cho thị trường toàn cầu.

                      </div>"),
                 column(width = 12, plotlyOutput("plot1")),
                 HTML("<br>"),
                 HTML("<br>"),
                 
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Tuy nhiên, những năm gần đây sự tăng trưởng về năng suất lúa gạo đã chậm lại ở một số quốc gia lớn như Indonesia, Myanmar, Thái Lan và Việt Nam. Đồng thời, diện tích trồng lúa gạo không có sự gia tăng đáng kể thậm chí có dấu hiệu giảm ở một số quốc gia, cùng với đó là việc phải đối diện với nguy cơ chuyển đổi đất trồng lúa sang sử dụng cho mục đích khác như dân dụng và công nghiệp. Mở rộng diện tích trồng lúa gặp khó khăn do thiếu đầu tư vào hạ tầng thủy lợi, khan hiếm nước và khó khăn về mặt kinh tế, môi trường. Hơn nữa, việc tăng cường cường độ trồng trọt cũng gặp không ít thách thức do sự hạn chế về số lượng vụ mùa trong năm. Mặc dù đã có những nỗ lực trong việc duy trì sản xuất lúa trong các hệ thống độc canh thâm canh, nhưng việc tiếp tục nâng cao năng suất trở nên khó khăn, thậm chí với các giống cây và công nghệ hiện đại nhất. Câu hỏi đặt ra là làm thế nào để các quốc gia Đông Nam Á có thể tiếp tục duy trì và củng cố vị thế của mình là nhà xuất khẩu gạo quan trọng trên thị trường toàn cầu.

                      </div>"),
                 column(width = 12, plotlyOutput("plot2")),
                 HTML("<br>"),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Trong giai đoạn 2019-2020, tỷ lệ tự cung cấp gạo (SSR) trung bình ở Đông Nam Á là 1,10, với thặng dư ước tính là 17 triệu tấn. Tuy nhiên, Indonesia và Philippines có SSR dưới 1(sản xuất không bằng nhu cầu) và thặng dư âm, buộc họ phải nhập khẩu gạo để đáp ứng nhu cầu trong nước. Các hạn chế như tốc độ tăng trưởng dân số và khí hậu nông nghiệp đang làm Indonesia và Philippines gặp khó khăn trong sản xuất gạo. Tương lai của Đông Nam Á là khu vực xuất khẩu gạo sẽ phụ thuộc vào thay đổi về năng suất và diện tích trồng gạo.

                      </div>"),
                 column(width = 12, plotlyOutput("plot3")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Mối lo ngại về tình trạng thiếu gạo hiện đang quay trở lại với một số quốc gia ở Đông Nam Á. Đông Nam Á sẽ không thể sản xuất lượng gạo dư thừa lớn trong tương lai nếu cứ tiếp tục với tốc độ tăng năng suất lúa như hàng năm gần đây. Việc không tăng năng suất trên diện tích đất trồng trọt hiện tại sẽ làm giảm đáng kể xuất khẩu gạo sang các khu vực khác đồng thời cũng giảm việc nhiều quốc gia trong khu vực đạt được hoặc duy trì khả năng tự cung cấp gạo. Điều đó đồng nghĩa là nhiều quốc gia trong ASEAN sẽ cần phải dựa vào thương mại khu vực để đáp ứng nhu cầu gạo trong nước. 

                      </div>"),
                 column(width = 12, plotlyOutput("plot4")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Chính phủ của nhiều nước ở Đông Nam Á đặt mục tiêu đảm bảo giá lương thực ổn định, không phụ thuộc hoàn toàn vào nhập khẩu gạo trong tương lai và tăng thu nhập từ xuất khẩu. Để đạt được điều này, các khoản đầu tư chiến lược vào chính sách nông nghiệp, đổi mới, nghiên cứu và phát triển là cần thiết để tăng năng suất trong vòng 20 năm tới. Hiện nay, Đông Nam Á có tiềm năng năng suất lúa gạo chưa được khai thác, đặc biệt là ở Campuchia, Myanmar, Philippines và Thái Lan, nơi mà năng suất hiện tại chỉ chiếm từ 50–70% của tiềm năng. Sự duy trì khả năng sản xuất gạo dư lớn của Đông Nam Á cũng có tầm quan trọng toàn cầu, giúp giảm biến động giá và cung cấp nguồn gạo ổn định và giá cả phải chăng cho các quốc gia khác, đặc biệt là ở châu Phi cận Sahara và Trung Đông.</div>"),
                 column(width = 12, plotlyOutput("plot5")),
                 HTML("<br>"),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'> <b>2. Năng lực cạnh tranh của chuối ở các nước thuộc khu vực Đông Nam Á </b>
                 <br>
              
                 Chuối là một trong những loại cây trồng phổ biến được trồng trên 120 quốc gia và được xem là mặt hàng trái cây sản xuất nhiều thứ hai trên thế giới sau cam. Với khả năng chống chịu biến đổi khí hậu tốt, chuối đang trở thành một trong những mặt hàng quan trọng trong ngành nông nghiệp của khu vực Đông Nam Á. Việt Nam và các quốc gia ASEAN khác là những nước sản xuất chuối hàng đầu trên thế giới, nhờ vào điều kiện khí hậu ấm áp, ẩm ướt quanh năm. Sản phẩm này không chỉ đóng góp vào tổng sản phẩm quốc nội (GDP) của khu vực mà còn tạo ra một nguồn thu nhập quan trọng cho người nông dân.

                      </div>"),
                 column(width = 12, plotlyOutput("plot6")),
                 HTML("<br>"),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Trải qua giai đoạn từ năm 2015 đến 2021, chuối là loại trái cây có năng suất cao nhất trong ba loại trái cây nhiệt đới nổi bật ở khu vực Đông Nam Á so với dừa và nhóm xoài, ổi, măng cụt. Đặc biệt, Indonesia đứng đầu về năng suất chuối vượt trội so với các đối thủ như Việt Nam, Malaysia và Thái Lan. Với điều kiện khí hậu thuận lợi và thành công trong việc đầu tư vào nghiên cứu và công nghệ, tạo điều kiện thuận lợi cho sự phát triển và tối ưu hóa sản xuất chuối của Indonesia, điều này đã mang lại lợi ích kinh tế và nâng cao chất lượng cuộc sống của cộng đồng nông dân và phát triển bền vững cho ngành nông nghiệp.
                 Về tình hình đất nông nghiệp, từ năm 2015 đến năm 2021, Thái Lan là nước chiếm ưu thế về diện tích đất được sử dụng cho nông nghiệp, với hơn 40% diện tích đất được sử dụng cho mục đích kinh tế nông nghiệp với nhiều loại hàng hóa trong đó có chuối. Việt Nam cũng có cải thiện đáng kể trong tỷ lệ đất nông nghiệp từ năm 2015 đến 2021, đạt gần 49% diện tích. Indonesia đứng thứ ba về tỷ lệ sử dụng đất cho nông nghiệp, trong khi Malaysia có tỷ lệ thấp nhất không bao giờ vượt quá 30% trong giai đoạn này.

                      </div>"),
                 column(width = 12, plotlyOutput("plot7")),
                 HTML("<br>"),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Thái Lan có diện tích đất sử dụng cho nông nghiệp lớn, nhưng sản lượng chuối xuất khẩu của họ lại thấp hơn các đối thủ cạnh tranh do sự ưu tiên của Thái Lan trong việc sử dụng đất và nguồn lực cho các mặt hàng nông sản khác như cà phê, cao su và các loại trái cây khác. thị trường như Hàn Quốc, Singapore, Malaysia, Trung Đông, Nga và Trung Quốc. Indonesia cũng là một quốc gia xuất khẩu chuối hàng đầu thế giới, đáp ứng nhu cầu toàn cầu. Giá trị xuất khẩu chuối đến Malaysia ước tính đạt 1,9 triệu USD, tăng nhẹ 5% so với cùng kỳ 2021.
                  </div>"),
                 column(width = 12, plotlyOutput("plot8")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Việc xuất khẩu chuối của Việt Nam tập trung vào nhiều loại chuối khác nhau và đã đạt giá trị cao khi năm 2022 đạt hơn 310 triệu USD, tăng trưởng 34,5% so với năm 2021 và xuất khẩu sang nhiều thị trường như Hàn Quốc, Singapore, Malaysia, Trung Đông, Nga và Trung Quốc. Indonesia cũng là một quốc gia xuất khẩu chuối hàng đầu thế giới, đáp ứng nhu cầu toàn cầu. Giá trị xuất khẩu chuối đến Malaysia ước tính đạt 1,9 triệu USD, tăng nhẹ 5% so với cùng kỳ 2021. Việt Nam hiện đang duy trì lợi thế cạnh tranh và giữ vị trí hàng đầu trong xuất khẩu chuối trong khu vực ASEAN.

                      </div>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>
                 <br>
                 <br><b>3.Cao su - tác nhân của nạn phá rừng ở các quốc gia Đông Nam Á. </b>
                 <br>
                 Nông nghiệp là một lĩnh vực rất quan trọng đối với các nước Đông Nam Á, chiếm hơn 25% GDP ở nhiều quốc gia và cung cấp hơn 40% tổng số việc làm tại Myanmar. ASEAN là nguồn cung cấp nông sản chính cho nhiều thị trường lớn trên thế giới. Indonesia, Thái Lan và Malaysia sản xuất khoảng 3,3 triệu tấn cao su mỗi năm, chiếm khoảng 70% sản lượng thế giới. Tuy nhiên, việc sản xuất cao su đã gây mất diện tích rừng đáng kể ở Đông Nam Á, theo nghiên cứu được công bố trên tạp chí Nature cho thấy hơn 4 triệu ha rừng đã bị phá hủy để trồng cao su kể từ năm 1993, và gần 3/4 diện tích rừng bị chặt phá từ năm 2001 trở đi.

                      </div>"),
                 column(width = 12, plotlyOutput("plot9")),
                 HTML("<br>"),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Xét theo từng quốc gia, kể từ năm 2001, nạn phá rừng cao nhất ở Indonesia, tiếp theo là Thái Lan và Malaysia, ba quốc gia này chiếm hơn 2/3 tổng số vụ phá rừng liên quan đến cao su ở Đông Nam Á trong giai đoạn 2001–2016, nạn phá rừng đáng kể cũng xảy ra ở Campuchia kể từ năm 2001, nơi có hơn 40% diện tích trồng cao su có liên quan đến nạn phá rừng. Tỷ lệ mở rộng cao su và nạn phá rừng liên quan đến các quyết định của hàng triệu người và bị ảnh hưởng bởi các động lực phức tạp và liên kết với nhau như chính sách và trợ cấp quốc gia, giá các loại cây trồng khác cũng như cơ sở hạ tầng. Tuy nhiên, điều đáng chú ý là ở một số quốc gia như Campuchia và Việt Nam tỷ lệ phá rừng liên quan đến cao su tăng cùng với sự tăng giá cao su toàn cầu từ năm 2000.</div>"),
                 column(width = 12, plotlyOutput("plot10")),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Dự đoán trong tương lai cho thấy nhu cầu về cao su tự nhiên có thể tiếp tục tăng do các chất thay thế không thể thay thế được. Cao su tự nhiên là nguồn tài nguyên tái tạo có tiềm năng giảm nhẹ biến đổi khí hậu và mang lại lợi ích kinh tế cho nông dân. Tuy nhiên, việc trồng cao su dẫn đến phá rừng có thể gây hậu quả nghiêm trọng đối với môi trường và sự phát triển bền vững. Vì vậy, EU đã ban hành Quy định chống mất rừng (EUDR) cấm nhập khẩu nông sản gây mất rừng vào EU, nhằm cải thiện minh bạch và bảo vệ môi trường. Các nước xuất khẩu cao su cần tuân thủ các quy định này để giải quyết nạn phá rừng.
                 <br>
                 <br>
                 <b>4. Chăn nuôi lợn suy giảm do sự bùng phát và lây lan của dịch tả lợn châu Phi (ASF) tại Đông Nam Á từ sau năm 2018. </b>
                 <br>
                 
                 Lợn là một trong những loài động vật được nuôi rộng rãi nhất trên thế giới và thịt lợn đã đóng vai trò quan trọng trong chế độ ăn của con người trong hàng ngàn năm. Với nhu cầu ngày càng tăng về các sản phẩm động vật, thịt lợn sẽ tiếp tục là một trong những nguồn cung cấp protein chính ở Đông Nam Á. Năm 2021, toàn thế giới có khoảng 749,62 triệu con lợn, trong đó Đông Nam Á chiếm tỷ lệ gần 10%. Từ năm 2015-2017, giết mổ lợn ở ĐNÁ vượt 110 triệu con/năm và sản lượng thịt lợn hàng năm vượt gần 7 triệu tấn (thịt lợn có xương, thịt lợn tươi hoặc ướp lạnh). Tuy nhiên, do sự bùng phát dịch tả lợn châu Phi (ASF) vào năm 2018 nên sản lượng thịt lợn hàng năm xuống còn 6 triệu tấn và dịch Covid-19 vào năm 2019 - 2020, sản lượng thịt lợn ở Đông Nam Á đã giảm đáng kể. Sau năm 2020, chăn nuôi lợn ở Đông Nam Á đã bắt đầu hồi phục, với tỷ lệ giết mổ lợn và sản lượng thịt lợn tăng trở lại so với năm 2020.


                      </div>"),
                 column(width = 12, plotlyOutput("plot11")),
                 HTML("<br>"),
                 HTML("<br>
                      <br>
                      <br>"),
                 
                 column(width = 12, plotlyOutput("plot12")),
                 HTML("<br>"),
                 HTML("<br>"),
                 
                 column(width = 12, plotlyOutput("plot13")),
                 HTML("<br>"),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Chịu ảnh hưởng của thói quen ăn kiêng, nguồn tài nguyên và các yếu tố tôn giáo và văn hóa, chăn nuôi lợn ở ĐNA thể hiện các đặc điểm phân cụm khu vực riêng biệt, chủ yếu tập trung nhiều ở Việt Nam, Philippines, Thái Lan và Indonesia. Không có gì đáng ngạc nhiên khi Việt Nam là nước sản xuất lợn hàng đầu ở ĐNA, đứng thứ 2 là Philippines và kể từ sau dịch Covid thì Indonesia đã giảm dần và đứng sau Thái Lan. Đến năm 2022, đàn lợn của Việt Nam đạt 23,533 triệu con, chiếm lần lượt 3,7% tổng đàn lợn Đông Nam Á
                      <br>
                      <br>
                      <b>5. Gia cầm </b>
                      <br>
                   
                      Tuy chăn nuôi  gia súc chưa trở thành ngành chính, nhưng Đông Nam Á là khu vực nuôi nhiều gia cầm: gà, vịt,.. Thái Lan là quốc gia có sản lượng thịt gà cao nhất trong khu vực giai đoạn 2013 - 2022, đạt hơn 1,5 triệu tấn mỗi năm, trong khi sản lượng thịt vịt lại thấp hơn, không đạt đến 200 nghìn tấn mỗi năm. Sự chênh lệch này có thể do nhiều yếu tố như nhu cầu tiêu dùng, điều kiện chăn nuôi, chính sách và văn hóa ẩm thực. Chính phủ Thái Lan đã áp dụng nhiều chính sách hỗ trợ cho ngành chăn nuôi gà, bao gồm cung cấp giống gà tốt, hỗ trợ kỹ thuật chăn nuôi và tìm kiếm thị trường xuất khẩu. Malaysia đứng ở vị trí thứ hai với khoảng gần 1 triệu 7 tấn thịt gà mỗi năm, nhưng sản lượng thịt vịt đã giảm dần. Việt Nam có mức tăng trưởng thịt gà vượt trội vào năm 2021, đạt gần 1,5 triệu tấn thịt, và cũng là quốc gia có sản lượng thịt vịt cao nhất trong khu vực ASEAN. 

                      </div>"),
                 column(width = 12, plotlyOutput("plot14")),
                 HTML("<br>"),
                 HTML("<br>"),
                 
                 column(width = 12, plotlyOutput("plot15")),
                 HTML("<br>"),
                 HTML("<br>"),
                 
                 column(width = 12, plotlyOutput("plot16")),
                 HTML("<br>"),
                 HTML("<br>"),
                 HTML("<br>"),
                 
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Tỷ lệ sản lượng thịt bò và trâu giảm so với Châu Á: Tỷ lệ sản lượng thịt bò và thịt trâu của Đông Nam Á so với châu Á đã có xu hướng giảm từ năm 2017, giảm từ khoảng 8.6% xuống còn khoảng 6.25%. Nguyên nhân chính bao gồm sự thay đổi trên thị trường, thách thức về chi phí, dịch bệnh và ảnh hưởng của biến đổi khí hậu. Sự thay đổi trên thị trường có thể do sự thay đổi trong nhu cầu tiêu thụ và chi phí chăn nuôi cao. Dịch bệnh như dịch lở mồm long móng đã gây thiệt hại lớn cho ngành chăn nuôi trâu bò ở nhiều quốc gia Đông Nam Á. Biến đổi khí hậu như hạn hán và lũ lụt cũng ảnh hưởng đến nguồn thức ăn và nước uống của trâu bò. Chính phủ cũng có những chính sách quan trọng trong việc hỗ trợ hoặc kiểm soát sản lượng gia súc để duy trì cân bằng thị trường. 
                 <br>
                 <br>
                 Tỷ lệ sản lượng gia cầm ở khu vực Đông Nam Á tăng nhẹ so với Châu Á: Từ 16,4% năm 2010 tăng lên tới 17,06% năm 2022 với mức đỉnh điểm đạt 18,84% năm 2017. Xu hướng tăng này có thể được giải thích bởi một số nguyên nhân sau đây. Thứ nhất, nhu cầu tiêu thụ gia cầm ngày càng tăng: Do dân số gia tăng và thu nhập bình quân đầu người cải thiện, nhu cầu tiêu thụ thịt gia cầm ở các nước Đông Nam Á cũng tăng cao, Thứ hai, sự phát triển của ngành công nghiệp chế biến thực phẩm: Ngành công nghiệp chế biến thực phẩm phát triển mạnh mẽ đã tạo ra nhu cầu lớn về nguyên liệu thịt gia cầm. Thứ ba, chính sách hỗ trợ của chính phủ: Một số quốc gia Đông Nam Á đã có những chính sách hỗ trợ cho lĩnh vực chăn nuôi gia cầm, như đầu tư vào cơ sở hạ tầng, cung cấp giống tốt và hỗ trợ dịch vụ thú y. Cuối cùng, năng suất chăn nuôi gia cầm ngày càng được cải thiện: Nhờ áp dụng các tiến bộ khoa học kỹ thuật vào chăn nuôi, năng suất chăn nuôi gia cầm ở các nước Đông Nam Á đã được cải thiện đáng kể.
                 <br>
                      </div>")
               ),
               size = "l", style = "width: 95%;"
             ))
           },
           chapter2 = {
             showModal(modalDialog(
               title =  HTML("<b>CHƯƠNG 2: THƯƠNG MẠI NÔNG NGHIỆP</b>"),
               fluidRow(
                 tags$style(HTML("
        .custom-text {
          font-family: 'Roboto', sans-serif;
          font-size: 15px; /* Tăng cỡ chữ */
        }
      ")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>
                   <br>
                   Hiệp hội các Quốc gia Đông Nam Á -ASEAN được xác định là khu vực xuất khẩu nông sản lớn của thế giới. Trong những năm gần đây, dù chuỗi cung ứng toàn cầu đứt gãy do đại dịch Covid-19, chiến sự Nga - Ukraine,... nhưng các nước ASEAN vẫn đạt sự tăng trưởng tốt nhờ xuất khẩu nông sản. Gạo và cà phê là hai mặt hàng nông sản chính mà ASEAN xuất khẩu. Sự tăng trưởng ổn định của ngành nông nghiệp và việc cải thiện chất lượng sản phẩm đã giúp ASEAN mở rộng thị trường xuất khẩu.
                   <br>
                   <b>1.Lúa gạo </b>
                 
                      </div>"),
                 column(width = 12, plotlyOutput("plot17")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Là một khu vực với vị thế “vựa lúa gạo” trong khu vực Châu Á, chiếm khoảng 40% xuất khẩu gạo quốc tế, Đông Nam Á đã duy trì ổn định sản lượng gạo xuất khẩu trong những năm 2011 - 2018. Trong giai đoạn 2011-2018, sản lượng gạo xuất khẩu ổn định. Tuy nhiên, từ năm 2019, dịch Covid-19 đã ảnh hưởng tiêu cực đến xuất khẩu gạo do tình hình dịch bệnh nghiêm trọng tại khu vực này, gây gián đoạn vận chuyển và thiếu container. Thêm vào đó, hiện tượng thời tiết cực đoan El Nino cũng gây ra thiếu hụt nguồn cung gạo.
                      <br>
                      <br>
                      <b>2.Cà phê</b>
                      </div>"),
                 column(width = 12, plotlyOutput("plot18")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Khí hậu và vị trí địa lý của Đông Nam Á là điều kiện lý tưởng cho canh tác cà phê. Hơn nữa, văn hóa cà phê khu vực và chuyên môn trong sản xuất cà phê đã góp phần tạo nên một ngành công nghiệp cà phê phát triển mạnh trong ASEAN. Một số nước Đông Nam Á đã trở thành “ông lớn” trong xuất khẩu cà phê như Việt Nam, Indonesia và Lào. 

                      </div>"),
                 column(width = 12, plotlyOutput("plot19")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Việt Nam là nước xuất khẩu cà phê hàng đầu trong ASEAN và là quốc gia xuất khẩu cà phê lớn thứ hai trên thế giới sau Brazil. Thị trường xuất khẩu cà phê chính là Đức, Mỹ và Ý trong khi nhập khẩu chủ yếu từ Lào, Indonesia, Brazil và Mỹ. Để tăng cường sản xuất cà phê, Chính phủ Việt Nam đã đặt mục tiêu có diện tích 500.000 ha đối với các đồn điền cà phê vào năm 2030, tập trung vào 4 tỉnh trọng điểm - Đăk Lăk, Lâm Đồng, Gia Nông và Gia Lai.
                      <br>
                      Indonesia cũng đạt được nhiều con số ấn tượng về ngành công nghiệp cà phê, trong suốt nhiều năm luôn giữ vững phong độ, là quốc gia có lượng cà phê xuất khẩu lớn thứ 4 thế giới. Indonesia nổi tiếng với loại cà phê đắt nhất thế giới có tên là Kopi Luwak, có nguồn gốc từ quả cà phê Cherry do loài cầy hương châu Á ăn và thải ra. Cà phê ở Indonesia được ủ trong túi vải và hiếm khi được rửa để giữ được hương vị khác biệt. Sau Việt Nam và Indonesia, Lào cũng là nhà sản xuất và xuất khẩu cà phê hàng đầu ở Đông Nam Á, đặc biệt là sản xuất cà phê Arabica chất lượng cao. Chính phủ Lào đang đề ra kế hoạch mở rộng sản xuất cà phê Arabica để cân bằng với sản lượng cà phê Robusta.
                      <br>
                      <br>
                      <b>3.Hạt điều</b>
                
                      </div>"),
                 column(width = 12, plotlyOutput("plot20")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Việt Nam là quốc gia hàng đầu về xuất khẩu hạt điều trên toàn thế giới, vượt qua tất cả các nước trong ASEAN. Kim ngạch xuất khẩu hạt điều của Việt Nam đã tăng đều qua các năm, đạt mốc 1 tỷ USD vào năm 2010 và lọt vào câu lạc bộ tỷ USD của ngành nông nghiệp. Trong những năm tiếp theo, xuất khẩu hạt điều của Việt Nam tiếp tục gia tăng, đạt 3,64 tỷ USD vào năm 2021, tăng 12,9% so với năm 2020. Mặc dù bị ảnh hưởng bởi đại dịch Covid-19 nhưng trong suốt 30 năm ngành điều Việt Nam vẫn luôn xuất siêu.
                      </div>"),
                 column(width = 12, plotlyOutput("plot21")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Mặc dù có Việt Nam đóng tỷ trọng lớn trong kim ngạch xuất khẩu hạt điều, thế nhưng tổng sản lượng điều được nhập khẩu tại ASEAN tăng mạnh. Lý giải cho điều này chính là vì Việt Nam chính là quốc gia nhập khẩu điều lớn tại ASEAN. Theo ông Phạm Văn Công, Chủ tịch Hiệp hội điều Việt Nam (VINACAS), dù là trung tâm chế biến điều nhân nhưng Việt Nam lại không có vùng nguyên liệu thô, diện tích vùng trồng trong nước và sản lượng ngày càng thu hẹp. Trong khi đó, các nước châu Phi đang có chính sách hạn chế bán điều thô để phát triển ngành chế biến điều trong nước. Vấn đề này càng tạo áp lực lên giá điều nguyên liệu. Một số nước đã đầu tư máy móc thiết bị chế biến điều, hướng đến thị trường Mỹ, EU… Dù chưa nhiều nhưng sản xuất ngay tại vùng nguyên liệu sẽ giảm được nhiều chi phí, giá thành tốt hơn. Đây chính là mấu chốt khiến các nhà máy Việt Nam phải cạnh tranh mua điều thô từ châu Phi, giá bán ra chịu sự cạnh tranh khốc liệt. Nếu Việt Nam không giải quyết được vấn đề nguồn cung nguyên liệu thô thì trong tương lai vị trí số 1 rất dễ bị “lung lay”.
                       <br>
                       <br>
                       <b>4.Rau quả</b>
                      </div>"),
                 column(width = 12, plotlyOutput("plot22")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Việt Nam và các quốc gia Đông Nam Á có điều kiện khí hậu và đất đai thuận lợi để phát triển sản xuất nông nghiệp, đặc biệt là rau củ chất lượng cao. Các loại rau này đáp ứng nhiều yêu cầu và thị hiếu khác nhau của người tiêu dùng trên toàn thế giới, được xuất khẩu đến hơn 60 quốc gia, bao gồm cả các thị trường khó tính như Mỹ, EU, Nhật Bản, Hàn Quốc, Australia, New Zealand. ASEAN là khu vực thị trường có sự đa dạng văn hóa nhưng vẫn thống nhất trong tổng thể. Sự tương đồng về thói quen tiêu dùng và văn hóa nông nghiệp giữa các quốc gia ASEAN làm cho mặt hàng rau củ từ khu vực này dễ dàng thâm nhập và được chào đón trên thị trường nội địa. Các mặt hàng xuất khẩu chủ yếu bao gồm các loại rau củ, rau họ đậu và họ hành.

                      <br>
                      <br>
                     <b> 5.Chăn nuôi</b>
                      </div>"),
                 column(width = 12, plotlyOutput("plot23")),
                 
                 column(width = 12, plotlyOutput("plot24")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'> Mặc dù ASEAN có lợi thế trong ngành chăn nuôi nhờ vào nền nông nghiệp cung cấp thức ăn chăn nuôi, nhưng vẫn nhập siêu thịt vì hai nguyên nhân chính. Thứ nhất, giá thành chăn nuôi tại ASEAN cao hơn so với các nước có nền chăn nuôi tiên tiến như Mỹ, Canada, châu  u hay Úc. Đồng thời, ngành chăn nuôi trong khu vực phụ thuộc vào nguồn thức ăn và thuốc thú y nhập khẩu, khiến ASEAN không thể cạnh tranh về giá thành. Thứ hai, các nước trong khu vực thực hiện bảo hộ ngành chăn nuôi mạnh mẽ, ví dụ như Nhật Bản, nơi các doanh nghiệp phải đàm phán lâu dài để xuất khẩu sản phẩm thịt. ASEAN cũng cần tập trung vào việc quy hoạch đất cho ngành chăn nuôi và cung cấp hỗ trợ tài chính để giúp các doanh nghiệp phát triển và vươn ra thị trường thế giới.
                      </div>"),
                 column(width = 12, plotlyOutput("plot25")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Số liệu từ biểu đồ cho thấy Thái Lan là quốc gia đứng vị trí số 1 về lượng và kim ngạch xuất khẩu gia cầm, bỏ xa các nước còn lại trong hiệp hội ASEAN. Năm 2023 Thái Lan cò trở thành nước xuất khẩu thịt gà lớn thứ 3 thế giới vươn lên 1 bậc so với năm 2022. Khác với các nước còn lại, Chính phủ Thái Lan đã đầu tư nguồn nhân lực rất lớn và tiềm lực tài chính rất mạnh để thực hiện các hoạt động thú y. Đặc biệt Thái Lan cung cấp đủ nguồn kinh phí để tổ chức các hoạt động thú y theo chuỗi. Hàng năm nước này cấp khoảng 180 triệu USD, tương đương khoảng 4.000 tỷ đồng để thực hiện các Chương trình phòng chống dịch bệnh động vật, đánh dấu nhận dạng gia súc, kiểm dịch vận chuyển, quản lý vận chuyển thông qua hệ thống trạm, chốt kiểm dịch và camera giám sát tuyến đường...

                      <br>
                      Đặc biệt từ sau dịch cúm gia cầm năm 2004, Thái Lan đã mạnh dạn chuyển đổi mô hình chăn nuôi. Đó là giảm chăn nuôi nông hộ tiềm ẩn nguy cơ cao dịch bệnh, đẩy mạnh chăn nuôi gia cầm công nghiệp, khép kín, có kiểm soát tại tất cả các khâu trong chuỗi sản xuất từ khâu sản xuất con giống, thức ăn, giết mổ, chế biến và bán sản phẩm ra thị trường. Đây là điều mà các quốc gia còn lại cần rút ra bài học từ ngành chăn nuôi gia cầm Thái Lan. 

                      </div>"),
                 column(width = 12, plotlyOutput("plot26")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Việt Nam là nước dẫn đầu ASEAN về chăn nuôi heo, có tổng trọng lượng xuất khẩu thịt heo lớn nhất trong khu vực. Ngành chăn nuôi heo của Việt Nam đóng góp 25,2% GDP ngành nông nghiệp và có tốc độ tăng trưởng khá cao. Tuy nhiên, việc tìm kiếm thị trường xuất khẩu cho thịt và các sản phẩm thịt vẫn là thách thức, khiến các doanh nghiệp lo lắng về công suất sẽ phải cắt giảm vì chưa có đầu ra ổn định ở thị trường nước ngoài.
                 <br>
                 Để phát triển chăn nuôi bền vững trong thời gian tới, thúc đẩy xuất khẩu thịt và sản phẩm thịt, Bộ Nông nghiệp và Phát triển nông thôn (NN&PTNT) sẽ tăng cường chuyển đổi số trong ngành chăn nuôi, nhằm cung cấp thông tin chính xác về năng suất sản xuất và cung cầu thị trường, giúp điều tiết sản xuất của các doanh nghiệp và người chăn nuôi; xây dựng sàn giao dịch nông sản thực phẩm.

                 <br>
                 Hiện Việt Nam chỉ thu về khoảng 45 triệu USD/năm từ xuất khẩu thịt lợn trong khi thị trường toàn cầu quy mô 28,5 tỷ USD. Để trở thành một cường quốc xuất khẩu thịt lợn, các chuyên gia cho rằng Việt Nam cần xây dựng chuỗi cung ứng đầy đủ và phát triển các thương hiệu thịt lợn Việt, đồng thời đảm bảo các tiêu chuẩn về an toàn dịch bệnh và chất lượng sản phẩm để phục vụ xuất khẩu.

                <br>
                <br>
                <b>6.Thủy sản</b>
                      </div>"),
                 column(width = 12, plotlyOutput("plot27")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Đông Nam Á có một hệ sinh thái biển đa dạng và là nguồn cung hải sản đa dạng về đánh bắt tự nhiên. Để đạt được sự phát triển bền vững trong ngành này, các nước như Việt Nam, Indonesia, Malaysia và Philippines đã nỗ lực để hỗ trợ và đảm bảo bền vững trong khai thác và nuôi trồng thủy sản.

                      </div>"),
                 column(width = 12, plotlyOutput("plot28")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Việt Nam là quốc gia xuất khẩu thủy sản lớn thứ ba trên thế giới tính đến năm 2022, với các mặt hàng chủ chốt như tôm, cá tra, cá ngừ, chiếm trên 7% thị phần toàn cầu, chỉ sau Trung Quốc và Na Uy.

                      <br>
                      Indonesia đang hướng tới nuôi trồng thủy sản bền vững, với khối lượng xuất khẩu tôm tăng từ 187.726 tấn năm 2021 lên 200.975 tấn năm 2022, và sản lượng tôm thẻ chân trắng đạt mức tương đương với Thái Lan, đứng sau Ecuador, Ấn Độ và Việt Nam.
                      <br)
                      Malaysia đang tăng cường nuôi trồng thủy sản để đáp ứng nhu cầu thị trường, với tốc độ tăng trưởng hàng năm là 4,6%.
                      <br>
                      Philippines đang sản xuất và nuôi trồng thủy sản trong điều kiện bình thường mới, với mức tăng trưởng ngành tôm gần 16% từ năm 2010 đến 2022.
                      <br>
                      Các nỗ lực này giúp các quốc gia trong khu vực Đông Nam Á tăng cường xuất khẩu và đáp ứng nhu cầu ngày càng tăng về thủy sản trên thị trường quốc tế.
                      <br>
                      <br>
                    <b> 7.Lâm nghiệp</b>
                      <br>
                      </div>"),
                 column(width = 12, plotlyOutput("plot29")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Không chỉ là quốc gia có kim ngạch xuất khẩu lâm sản thứ nhất Đông Nam Á, những năm gần đây Indonesia còn là quốc gia đứng top đầu thế giới về sản lượng xuất khẩu khẩu gỗ và các mặt hàng từ gỗ. Các quốc gia khác trong khu vực như Malaysia, Thái Lan và Việt Nam cũng có lợi thế về tài nguyên lâm sản phong phú và điều kiện tự nhiên thuận lợi, bao gồm cả các loại cây gỗ quý hiếm và phổ biến.  Các chính sách hỗ trợ như thuế nhập khẩu thấp hoặc miễn thuế đối với sản phẩm gỗ xuất khẩu và các biện pháp khuyến khích đầu tư trong ngành công nghiệp lâm sản đã thúc đẩy sự phát triển của ngành này. Nhu cầu ngày càng tăng về các sản phẩm đồ gỗ trong nhiều lĩnh vực như xây dựng, nội thất và đồ gia dụng trên toàn thế giới đã tạo ra một nền tảng ổn định và lớn cho các quốc gia xuất khẩu gỗ.Tất cả những yếu tố này đang mở ra triển vọng phát triển hứa hẹn cho ngành lâm sản ASEAN và các quốc gia trong khu vực.
                      </div>"),
                 column(width = 12, plotlyOutput("plot30")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Năm 2022, đây là một dấu mốc đáng nhớ, khi tổng kim ngạch xuất khẩu lâm sản của ASEAN đạt gần 25 tỷ USD.  Trong đó Indonesia ở vị trí dẫn đầu chiếm 41.5% tổng kim ngạch xuất khẩu, đứng ở vị trí thứ 2 là Việt Nam 18.5%, vị trí thứ 3 là Thái Lan với 15.3%.
                      </div>"),
                 column(width = 12, plotlyOutput("plot31")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Nhìn lại năm 2010, có thể thấy Việt Nam đã nỗ lực như thế nào để vươn lên vị trí thứ 2 về tổng kim ngạch xuất khẩu lâm sản trong khu vực ASEAN như thời điểm hiện tại. Theo số liệu Tổng cục Lâm nghiệp Việt Nam (2022), hiện nguồn nguyên liệu gỗ trong nước đáp ứng khoảng 70-75% cho sản xuất, chế biến đồ gỗ. Bên cạnh đó, việc gia tăng diện tích rừng trồng được cấp các loại chứng chỉ quản lý rừng đã góp phần nâng cao hình ảnh sản phẩm đồ gỗ của Việt Nam trên thị trường quốc tế.  
                 <br>
                 <br>
                 Lâm nghiệp đóng vai trò quan trọng trong lĩnh vực thương mại đối với các quốc gia ASEAN. Thế nhưng ngành lâm nghiệp các quốc gia thành viên ASEAN đang phải đối mặt với nhiều khó khăn. Dễ nhận thấy nhất là việc các nước chưa đủ khả năng giải quyết triệt để nạn khai thác và buôn gỗ lậu, hậu quả là làm thất thoát nguồn thu và suy thoái rừng; Một số biện pháp giải quyết nghèo đói như chuyển đổi cơ cấu nông nghiệp và phát triển rừng trồng đi ngược với nhu cầu của người dân nông thôn. Sự kết nối kĩ thuật công nghệ mới trong trồng rừng và khai thác gỗ có thể mang lại năng suất cao hơn nhưng cũng làm cho nhu cầu lao động thấp hơn trên mỗi đơn vị sản phẩm đầu ra. Còn nhiều khoảng trống chưa được khỏa lấp trong các lĩnh vực hợp tác liên quan đến sự kết nối khu vực tư nhân, hợp tác xã nông nghiệp, nghiên cứu - phát triển và chuyển giao công nghệ,..

                      </div>"),
                 column(width = 12, plotlyOutput("plot32")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Các sản phẩm xuất khẩu lâm sản của ASEAN chủ yếu đóng vai trò là nguồn cung cấp nguyên liệu thô, song lại đi nhập khẩu các sản phẩm thủ công mỹ nghệ,  đồ gỗ cao cấp từ Trung Quốc, Mỹ,..chính vì lý do này mà không có sự chênh lệch quá lớn giữa nhập khẩu và xuất khẩu lâm sản. Về lâu dài, nguồn cung sẽ trở thành một yếu tố ảnh hưởng đến khả năng cạnh tranh của ngành lâm nghiệp, đặc biệt trong trường hợp quỹ đất trồng rừng ngày càng hạn hẹp do các quốc gia mở rộng nhượng quyền cho các doanh nghiệp trồng dầu cọ - loại cây công nghiệp mang lại doanh thu cao hơn trên cùng một diện tích đất trồng. Do đó, các quốc gia cần có những giải pháp kịp thời để giải quyết các khó khăn cũng như phát triển bền vững ngành lâm nghiệp. 

                      </div>")
                 
               ),
               size= "l", style = "width: 95%;"
             ))
           },
           chapter3 = {
             showModal(modalDialog(
               title =  HTML("<b>CHƯƠNG 3: AN NINH LƯƠNG THỰC</b>"),
               fluidRow(
                 tags$style(HTML("
        .custom-text {
          font-family: 'Roboto', sans-serif;
          font-size: 15px; /* Tăng cỡ chữ */
        }
      ")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'><b>1. Tình hình chung về bối cảnh an ninh lương thực và tình trạng đói nghèo tại các quốc gia Đông Nam Á</b>
                <br>
               
                Trong vài thập kỷ qua, Đông Nam Á đã đạt được tiến bộ đáng kể trong việc giảm đói và suy dinh dưỡng. Khu vực này đã nhận được sự hỗ trợ từ các nền kinh tế nông nghiệp phát triển trên thế giới, tuy nhiên lại phải đối mặt với nguy cơ mất an ninh lương thực toàn cầu, đặc biệt ở các vùng sâu vùng xa.
                      <br>
                      <br>
                      Tuy tỷ lệ người thiếu dinh dưỡng trong dân số phản ánh tỷ lệ nghèo đói nhưng nó không nói lên điều gì về mức độ thiếu dinh dưỡng - “độ sâu” của tình trạng thiếu hụt lương thực, được đo bằng kCal/capita/day. Cùng với việc giảm tỷ lệ suy dinh dưỡng, mức độ thâm hụt lương thực cũng đã giảm hơn 10 năm qua. Trong số các nền kinh tế châu Á, đặc biệt ở Đông Nam Á, có thể thấy được sự sụt giảm đáng kể. Ở các quốc gia như Việt Nam, Malaysia, Thái Lan,... sự thâm hụt lương thực giảm khoảng 14%.
                      
                      </div>"),
                 HTML("<br>"),
                 column(width = 12, plotlyOutput("plot33")),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Theo báo cáo về an ninh lương thực các nước khu vực châu Á - Thái Bình Dương của FAO, tỷ lệ dân số suy dinh dưỡng của Việt Nam đã giảm 79% so với giai đoạn 2014-2016 với giai đoạn 1990-1992, tốc độ giảm nhanh nhất trong khu vực so với Thái Lan và Myanmar. An ninh lương thực ở Việt Nam có đặc điểm là tự cung tự cấp lương thực và cải thiện khả năng tiếp cận lương thực nhưng việc sử dụng lương thực không đạt yêu cầu trong những năm qua và đang bị thách thức bởi các hiện tượng khí hậu cực đoan, như lũ lụt và hạn hán nghiêm trọng. Dựa trên các xu hướng hiện tại, thế giới sẽ còn rất xa mới đạt được Mục tiêu Phát triển Bền vững số 2 nhằm đạt mục tiêu “Không còn nạn đói” vào năm 2030.
                      </div>"),
                 column(width = 12, plotlyOutput("plot34")),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Mặc dù mức độ sự giàu có ngày càng tăng ở châu Á, phần lớn dân số vẫn nghèo đói, và các chỉ số như tình trạng suy dinh dưỡng ở bà mẹ và trẻ em cho thấy khu vực này đang tụt hậu trong việc đạt được an ninh dinh dưỡng trong khi trọng tâm của an ninh lương thực trong khu vực là đáp ứng được nhu cầu dinh dưỡng, đặc biệt là ở các nhóm dễ bị tổn thương.

                      </div>"),
                 column(width = 12, plotlyOutput("plot35")),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Tỷ lệ nghèo giảm đáng kể nhưng không tương xứng với mức giảm tương ứng của tỷ lệ suy dinh dưỡng khi có 7,3% dân số trong khu vực Đông Nam Á bị suy dinh dưỡng, 18,8% dân số phải đối mặt với tình trạng mất an ninh lương thực ở mức độ vừa hoặc nghiêm trọng vào năm 2020 so với mục tiêu dinh dưỡng toàn cầu.
                      
                      <br>
                      <br>
                      </div>"),
                 column(width = 12, plotlyOutput("plot36")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'>Đây cũng là bằng chứng cho thấy lạm phát giá lương thực và giảm thu nhập đã buộc nhiều hộ gia đình có thu nhập thấp ở các nước như Lào, Malaysia và Campuchia phải tiêu dùng thực phẩm rẻ hơn nhưng  kém dinh dinh dưỡng hơn những năm gần đây. 
                      
                      <br>
                      <br>
                      </div>"),
                
                 HTML("<div style='margin-left: 50px;' class='custom-text'><b>2. Bức tranh đáng báo động về mức độ suy dinh dưỡng của trẻ em</b>
                 <br>
                 
                 Tính đến năm 2020, 27,4% trẻ em dưới 5 tuổi ở Đông Nam Á - hầu hết xuất thân từ gia đình nghèo và khu vực nông thôn - bị  chậm phát triển. Dựa trên khảo sát cho thấy mức độ thấp còi cao - chiều cao theo tuổi cao hơn hai độ lệch chuẩn dưới mức trung bình của Tiêu chuẩn Tăng trưởng Trẻ em của WHO - ở trẻ em dưới 5 tuổi một số khu vực ở Đông Nam Á, tình trạng thiếu máu cao (hemoglobin trong máu thấp do thiếu sắt) và thiếu vitamin A tồn tại tỷ lệ cao ở các quốc gia như Campuchia, Myanmar, Indonesia, Philippines và Việt Nam. 
                      
                      </div>"),
                 column(width = 12, plotlyOutput("plot37")),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text' >Hơn 4 triệu trẻ em dưới 5 tuổi bị suy dinh dưỡng thể gầy ở khu vực ASEAN và Timor-Leste là quốc gia thành viên ASEAN có tỷ lệ thấp còi ở mức cao. Tuy nhiên, tỷ lệ thấp còi được coi là ở mức “trung bình” ở các quốc gia thành viên (Việt Nam, Campuchia, Indonesia, Cộng hòa Dân chủ Nhân dân Lào, Malaysia, Myanmar, Philippines và Thái Lan). Ở Việt Nam tỷ lệ suy dinh dưỡng giảm chậm và đều đặn nhưng vẫn là gánh nặng đối với đất nước ở thời điểm hiện tại khi tỷ lệ suy dinh dưỡng ở trẻ dưới 5 tuổi vẫn là 24,6% thấp còi, 14,1% nhẹ cân và 6,4% gầy còn (NIN, 2015).

                      </div>"),
                 column(width = 12, plotlyOutput("plot38")),
                 HTML("<br>"),
                 HTML("<div style='margin-left: 50px;' class='custom-text' >Khi vấn đề đang nổi lên, kiểm soát hiệu quả chính là chìa khóa. Thách thức ở đây là phát triển các chương trình và chính sách hiệu quả cụ thể phù hợp với bối cảnh của một quốc gia. Ví dụ, giảm tình trạng suy dinh dưỡng và thiếu hụt chất dinh dưỡng ở trẻ em và người lớn vẫn là ưu tiên hàng đầu ở Timor-Leste, Indonesia, Philippines. 

                      <br>
                      <br>
                      Với hơn 60% dân số dựa vào nông nghiệp và sản xuất lương thực làm nguồn thu nhập, Đông Nam Á nói chung và Việt Nam nói riêng hiện tại đặc biệt nhạy cảm với những thiệt hại tiềm tàng do biến đổi khí hậu gây ra. Vì vậy, nhu cầu giải quyết các tác động của biến đổi khí hậu đối với an ninh lương thực là cấp thiết - đòi hỏi phải có phản ứng ngay lập tức và phù hợp. Các mô hình sản xuất ở Việt Nam chủ yếu dựa vào nông nghiệp đầu vào cao, tác động tiêu cực đến hệ sinh thái môi trường. Việt Nam hiện đang chuyển đổi sang nền nông nghiệp xanh, ít cacbon, thích ứng với biến đổi khí hậu. Sự chuyển đổi dự kiến ​​sẽ tạo ra cơ hội việc làm mới. Đáng chú ý nhất là ngành trồng lúa chuyển đổi có khả năng tạo ra hàng triệu việc làm mới, đặc biệt là cho giới trẻ.
                      </div>")
                 
               ),
               size= "l", style = "width: 95%;"
             ))
           },
           chapter4 = {
             showModal(modalDialog(
               title =  HTML("<b>CHƯƠNG 4: PHÁT TRIỂN BỀN VỮNG</b>"),
               fluidRow(
                 tags$style(HTML("
        .custom-text {
          font-family: 'Roboto', sans-serif;
          font-size: 15px; /* Tăng cỡ chữ */
        }
      ")),
                 HTML("<div style='margin-left: 50px;' class='custom-text'> <b>1. Ảnh hưởng của biến động thời tiết đến sản lượng sản phẩm.</b>
                      </div>"),
                 column(width = 12, plotlyOutput("plot39")),
                 HTML("<div style='margin-left: 50px;' class='custom-text' >ASEAN có một nền nông nghiệp phong phú, lớn mạnh, chủ yếu dựa vào các nguồn lực thiên nhiên. Tuy nhiên, sự gia tăng của hiện tượng nóng lên toàn cầu từ năm 2010 đã gây ra những thách thức không nhỏ, khi mà sự biến đổi về nhiệt độ đã ảnh hưởng trực tiếp đến sản lượng xuất khẩu của các sản phẩm chủ lực như cà phê xanh, cao su tự nhiên, dừa và gạo. Trong suốt hơn 20 năm qua, việc giảm kỷ luật từ năm 2010 đã làm sản lượng của các mặt hàng đó xuống mức thấp hơn. Hiện nay, sản lượng tạo ra không thể sánh kịp với những gì đã được sản xuất trong những thập kỷ trước, đặc biệt là những năm 90.
                 <br>
                 <br>
                
                <b> 2. Câu chuyện về việc hạn chế thuốc bảo vệ thực vật của Thái Lan</b>
                      </div>"),
                 column(width = 12, plotlyOutput("plot40")),
                 HTML("<div style='margin-left: 50px;' class='custom-text' >Từ năm 2012 trở về trước, Đông Nam Á đã tiêu thụ thuốc trừ sâu mạnh mẽ do các chính sách ưu đãi thuế nhập khẩu và khuyến khích sử dụng thuốc trừ sâu để tăng sản lượng. Tuy nhiên, việc sử dụng thuốc trừ sâu không đúng cách đã khiến dư lượng thuốc trừ sâu trong sản phẩm vượt quá mức cho phép.

                      <br>
                      Song, từ năm 2012 trở đi, Thái Lan đã tiên phong trong việc nâng cao chất lượng sản phẩm thông qua việc ban hành các luật lệ và chương trình như Quản lý dịch hại tổng hợp (IPM) và Thực hành nông nghiệp tốt (GAP). Nhờ những nỗ lực này, Thái Lan đã giảm đáng kể việc lạm dụng các hoá chất nông nghiệp, từ 3.96kg/ha xuống còn 0.37kg/ha chỉ trong 2 năm.
                      <br>
                      <br>
                      
                      <b>3. Việt Nam chưa tận dụng được các ưu đãi về thuế quan</b>

                      </div>"),
                 column(width = 12, plotlyOutput("plot41")),
                 HTML("<div style='margin-left: 50px;' class='custom-text' >Sau khi Hiệp định EVFTA có hiệu lực, tỷ lệ giảm thuế của hàng nông sản Việt Nam vào thị trường EU là 94%, nhưng tăng trưởng xuất khẩu chậm do không đáp ứng yêu cầu về vệ sinh an toàn thực phẩm. Cụ thể, vào tháng 7/2023 các cơ quan địa phương ở Nhật Bản đã cảnh báo Việt Nam về 107 vi phạm các lô hàng xuất khẩu không đáp ứng yêu cầu vệ sinh an toàn thực phẩm. Và sự việc tiếp tục tiếp diễn đến tháng 9/2023, 2 lô hàng sầu riêng và ớt ở nước ta đã bị Nhật Bản tiêu hủy do tồn dư một số chất cấm.
                 <br>
                 Nếu không tận dụng cơ hội từ các hiệp định thuế quan này thì sau 5 năm, nông sản Việt Nam sẽ khó cạnh tranh với các nước như Indonesia, Malaysia và Brazil, vốn là những nước đang ngày càng đổi mới trong nông nghiệp xanh.
                 <br>
                 <br>
                 <b>4. ASEAN vẫn còn đang loay hoay trong vấn đề chuyển đổi xanh.</b>

                      </div>"),
                 column(width = 12, plotlyOutput("plot42")),
                 HTML("<div style='margin-left: 50px;' class='custom-text' >ASEAN vẫn còn đang loay hoay trong vấn đề phát triển nông thôn thông qua các chiến lược ưu tiên phát triển, tập trung vào lĩnh vực như xoá đói giảm nghèo, xoá nạn mù chữ, xây dựng đường xá và cơ sở hạ tầng. Các hội nghị của ASEAN cũng chỉ tập trung vào các vấn đề đảm bảo hệ thống an ninh lương thực, tạo ra sự thay đổi rõ rệt về diện mạo về nông thôn. Cụ thể như Việt Nam đang triển khai Chương trình mục tiêu quốc gia giảm nghèo bền vững giai đoạn 2021-2025 với mục đích nhằm giảm bớt tình trạng nghèo ở các vùng, các khu vực khó khăn hay “Dự án Bajarasudha Gajanurak” tại Thái Lan thiết lập những mảnh đất bỏ hoang và thường xuyên bị voi phá hoại thành một nơi có thể canh tác và sinh sống,... Còn các vấn đề về “chuyển đổi xanh”, “nông nghiệp xanh” chỉ dừng lại ở việc xây dựng chính sách và phát triển, chưa có một nguồn nào nổi trội trong nỗi lực biến những điều ấy thành hiện thực, nguồn vốn vào nghiên cứu rất eo hẹp (chiếm 2.15%) nguồn vốn ưu tiên cho năng lượng sạch cũng rất hạn chế, lần lượt chiếm 8.38% và 5.19%
                 

                      </div>")
               ),
               size= "l", style = "width: 95%;"
             ))
           }
    )
  })
  
  # Define the plots
  output$plot1 <- renderPlotly({interactive_plotY})
  output$plot2 <- renderPlotly({interactive_plotS})
  output$plot3 <- renderPlotly({interactive_plotPotential})
  output$plot4 <- renderPlotly({ ssr_plot_interactive })
  output$plot5 <- renderPlotly( { ricesunplus_interactive})
  output$plot6 <- renderPlotly( { agricultural_land_interactive})
  output$plot7 <- renderPlotly( { fruit_interactive})
  output$plot8 <- renderPlotly( { export_interactive})
  output$plot9 <- renderPlotly( { rubber_interactive })
  output$plot10 <- renderPlotly( { pr })
  output$plot11 <- renderPlotly( { gm})
  output$plot12 <- renderPlotly( { sl})
  output$plot13 <- renderPlotly( { dl})
  output$plot14 <- renderPlotly( { gv})
  output$plot15 <- renderPlotly( { slgv})
  output$plot16 <- renderPlotly( { tlpt })
  
  output$plot17 <- renderPlotly( { DNA})
  output$plot18 <- renderPlotly( { cf})
  output$plot19 <- renderPlotly( { coffee_plot})
  output$plot20 <- renderPlotly( { plot_cashew_nuts})
  output$plot21 <- renderPlotly( { cashew_plot})
  output$plot22 <- renderPlotly( { raucu_plot })
  output$plot23 <- renderPlotly( { chart_pork})
  output$plot24 <- renderPlotly( { chart_poultry})
  output$plot25 <- renderPlotly( { chart_export_pork})
  output$plot26 <- renderPlotly( { chart_poultry2})
  output$plot27 <- renderPlotly( { interactive_pie_chart1})
  output$plot28 <- renderPlotly( { interactive_pie_chart_export})
  output$plot29 <- renderPlotly( { chart_1})
  output$plot30 <- renderPlotly( { interactive_ls})
  output$plot31 <- renderPlotly( { interactive_ls_2022})
  output$plot32 <- renderPlotly( { chart_2})
  
  
  output$plot33 <- renderPlotly( { calo_plot})
  output$plot34 <- renderPlotly( { a})
  output$plot35 <- renderPlotly( { b})
  output$plot36 <- renderPlotly( { c})
  output$plot37 <- renderPlotly( { thin_people})
  output$plot38 <- renderPlotly( { plotly_chart})
  
  output$plot39 <- renderPlotly( { temperature_plot})
  output$plot40 <- renderPlotly( { pesticides_plot})
  output$plot41 <- renderPlotly( { p})
  output$plot42 <- renderPlotly( { interactive_capital_purpose}) 
}

# Run the application
shinyApp(ui = ui, server = server)


