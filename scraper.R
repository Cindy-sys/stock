library(rvest)
library(dplyr)
library(stringr)
library(jsonlite)
library(lubridate)

url <- "https://banggia.vndirect.com.vn/chung-khoan/vn30"

# Tăng thời gian chờ lên 15-20s vì GitHub Actions chạy máy ảo nên mạng có thể chậm hơn máy thật
web <- read_html_live(url)
Sys.sleep(20) 

folder_path <- "./"
csv_path  <- file.path(folder_path, "bang_gia_vn30.csv")
json_path <- file.path(folder_path, "bang_gia_vn30.json")

tryCatch({
  # Lấy các dòng dữ liệu
  rows <- web %>% html_elements("tbody#banggia-khop-lenh-body tr")
  
  if (length(rows) == 0) {
    stop("Không tìm thấy dòng dữ liệu nào. Có thể thị trường đang đóng cửa hoặc web chưa tải xong.")
  }

  all_data <- data.frame()
  
  for (i in rows) {
    row <- i %>% html_elements('td') %>% html_text2()
    
    # Kiểm tra số lượng cột (Bảng VN30 thường có khoảng 26-27 cột)
    if(length(row) < 20) next
    
    new_row <- data.frame(
      Date = dmy_hms(format(Sys.time(), "%d/%m/%Y %H:%M:%S")),
      Ma_CK = as.character(row[1]),
      TC = as.numeric(row[2]),
      Tran = as.numeric(row[3]),
      San = as.numeric(row[4]),
      Tong_KL = as.numeric(gsub(",", "", unlist(str_split(row[5], " "))[1])), #Do phần này vừa có Vol và Val nên phải tách lấy Vol và xử lý dẩu ','
      Gia_mua_3 = as.numeric(as.numeric(row[6])),
      KL_mua_3 = as.numeric(gsub(",", "", row[7])), #Do KL dùng dấu ',' để phân cách hàng ngàn nên cần xử lý 
      Gia_mua_2 = as.numeric(row[8]),
      KL_mua_2 = as.numeric(gsub(",", "", row[9])),
      Gia_mua_1 = as.numeric(row[10]),
      KL_mua_1 = as.numeric(gsub(",", "", row[11])),
      Gia = as.numeric(row[12]),
      KL = as.numeric(gsub(",", "", row[13])),
      per = as.numeric(gsub("%", "", unlist(str_split(row[14], " "))[2])),
      Gia_ban_1 = as.numeric(row[15]),
      KL_ban_1 = as.numeric(gsub(",", "", row[16])),
      Gia_ban_2 = as.numeric(row[17]),
      KL_ban_2 = as.numeric(gsub(",", "", row[18])),
      Gia_ban_3 = as.numeric(row[19]),
      KL_ban_3 = as.numeric(gsub(",", "", row[20])),
      Gia_cao = as.numeric(row[21]),
      Gia_TB = as.numeric(row[22]),
      Gia_thap = as.numeric(row[23]),
      Du_mua = as.numeric(gsub(",", "", row[24])), #Hiện các mã không có dư mua, dư bán. Nên không chắc dữ liệu có dùng ','
      Du_ban = as.numeric(gsub(",", "", row[25])),
      DTNN_mua = as.numeric(gsub(",", "", unlist(str_split(row[26], " "))[1])),
      DTNN_ban = as.numeric(gsub(",", "", unlist(str_split(row[26], " "))[2])),
      stringsAsFactors = FALSE #Tránh bị label encoding
    )
    
    all_data <- rbind(all_data, new_row)
  }
  
  if (nrow(all_data) > 0) {
    # Lưu vào CSV
    write.table(all_data, file = csv_path, append = TRUE, sep = ",", 
                row.names = FALSE, quote = FALSE, 
                col.names = !file.exists(csv_path), fileEncoding = "UTF-8")
    
    # Lưu vào JSON
    con <- file(json_path, open = "a")
    stream_out(all_data, con, verbose = FALSE)
    close(con)
    
    print(paste("Thành công! Đã cập nhật", nrow(all_data), "mã vào lúc:", Sys.time()))
  }

}, error = function(e){
  print(paste("Lỗi cụ thể:", e$message))
})

