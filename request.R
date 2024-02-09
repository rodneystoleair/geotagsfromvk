library(httr2)
library(tidyverse)
library(writexl)

VK_ACCESS_TOKEN = 'vk1.a.2m8C-axmCes_i-dT29PxrEXwt0FzeSCLZFWb3X1W1rrzeyV9hy7zVebHHPckqNeng9kxrIR50TSo-qn9HS95NPQvzxlGjPDbAEYjOgWfO34EFDlrDcFRQW5Fe-WxI3LqLQtw3nIEuz74YhWhV5E7MCNIiF43F7ASFlfwP75IfAPEzbEbT_aZbFAOu9BEmJY8oOvgyfeDbuiHRy-0E3yKJw'
geo = c(56.838011, 60.597474)
dist = 12000
timeperiod = c(1672520400, 1704056400)
vk_version = '5.199'

get_vk = function(geo, timeperiod, offset) {
  params = list(
    q = 'парк',
    lat = geo[1],
    long = geo[2],
    count = '1000',
    offset = offset,
    radius = dist,
    start_time = timeperiod[1],
    end_time = timeperiod[2],
    access_token = VK_ACCESS_TOKEN,
    v = vk_version,
    sort = 0)
  req = request('https://api.vk.com/method/photos.search') |> 
    req_url_query(!!!params)
  resp = req |> 
    req_perform() |> 
    resp_body_json()
  return(resp)
}

save_as_df = function(resp) {
  resp1 = resp[["response"]][["items"]]
  data = data.frame(id = c(1:length(resp1)),
                    date = c(1:length(resp1)),
                    lat = c(1:length(resp1)),
                    lon = c(1:length(resp1)))
  for (i in 1:length(resp1)) {
    data$id[i] = resp1[[i]][["id"]]
    data$date[i] = resp1[[i]][["date"]]
    if (is.null(resp1[[i]][["lat"]]) == T) {
      data$lat[i] = NA
      data$lon[i] = NA
    } else {
      data$lat[i] = resp1[[i]][["lat"]]
      data$lon[i] = resp1[[i]][["long"]]
    }
  }
  return(drop_na(data))
}

step = 24*60*60
i = timeperiod[1]
data_df = data.frame(id = 1,
                     date = 1,
                     lat = 1,
                     lon = 1)
while (i < timeperiod[2]) {
  timeperiod_temp = c(i, i + step)
  resp_vk = get_vk(geo, timeperiod_temp, 0)
  df_temp = save_as_df(resp_vk)
  data_df = rbind(data_df, df_temp)
  count = resp_vk[["response"]][["count"]]
  returned = length(resp_vk[["response"]][["items"]])
  if (count > returned) {
    offset = returned
    while ((offset < count) && (offset < 3000)) {
      timeperiod_temp = c(i, i + step)
      resp_vk = get_vk(geo, timeperiod_temp, 0)
      df_temp = save_as_df(resp_vk)
      data_df = rbind(data_df, df_temp)
      count = resp_vk[["response"]][["count"]]
      returned = length(resp_vk[["response"]][["items"]])
      offset = offset + returned
      if (returned == 0) {
        break
      }
    }
  }
  i = i + step
  print(returned)
  Sys.sleep(0.5)
}

data_df = data_df |> 
  mutate(Datetime = as.Date(as.POSIXct(date, origin='1970-01-01')))

write.csv(data_df, 'output/geotags_ekb_2024.csv')
