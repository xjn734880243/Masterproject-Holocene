###################################################change solar#######################################
##Calculation of solar
library(palinsol)
`%>%` <- magrittr::`%>%`
mid_month <- seq(from = 1, to = 366, by = 1)
tt_present <- 0.0
orbit_present <- palinsol::astro(t = tt_present, 
                                 solution = palinsol::ber78, 
                                 degree = FALSE)
mid_month_tsl_present <- palinsol::day2l(orbit = orbit_present, 
                                         day = mid_month)
years <- -9000
names<-c("1",	"2",	"3",	"4",	"5",	"6",	"7",	"8",	"9",	"10",	"11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	"21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	"31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",	"41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	"51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	"61",	"62",	"63",	"64",	"65",	"66",	"67",	"68",	"69",	"70",	"71",	"72",	"73",	"74",	"75",	"76",	"77",	"78",	"79",	"80",	"81",	"82",	"83",	"84",	"85",	"86",	"87",	"88",	"89",	"90",	"91",	"92",	"93",	"94",	"95",	"96",	"97",	"98",	"99",	"100",	"101",	"102",	"103",	"104",	"105",	"106",	"107",	"108",	"109",	"110",	"111",	"112",	"113",	"114",	"115",	"116",	"117",	"118",	"119",	"120",	"121",	"122",	"123",	"124",	"125",	"126",	"127",	"128",	"129",	"130",	"131",	"132",	"133",	"134",	"135",	"136",	"137",	"138",	"139",	"140",	"141",	"142",	"143",	"144",	"145",	"146",	"147",	"148",	"149",	"150",	"151",	"152",	"153",	"154",	"155",	"156",	"157",	"158",	"159",	"160",	"161",	"162",	"163",	"164",	"165",	"166",	"167",	"168",	"169",	"170",	"171",	"172",	"173",	"174",	"175",	"176",	"177",	"178",	"179",	"180",	"181",	"182",	"183",	"184",	"185",	"186",	"187",	"188",	"189",	"190",	"191",	"192",	"193",	"194",	"195",	"196",	"197",	"198",	"199",	"200",	"201",	"202",	"203",	"204",	"205",	"206",	"207",	"208",	"209",	"210",	"211",	"212",	"213",	"214",	"215",	"216",	"217",	"218",	"219",	"220",	"221",	"222",	"223",	"224",	"225",	"226",	"227",	"228",	"229",	"230",	"231",	"232",	"233",	"234",	"235",	"236",	"237",	"238",	"239",	"240",	"241",	"242",	"243",	"244",	"245",	"246",	"247",	"248",	"249",	"250",	"251",	"252",	"253",	"254",	"255",	"256",	"257",	"258",	"259",	"260",	"261",	"262",	"263",	"264",	"265",	"266",	"267",	"268",	"269",	"270",	"271",	"272",	"273",	"274",	"275",	"276",	"277",	"278",	"279",	"280",	"281",	"282",	"283",	"284",	"285",	"286",	"287",	"288",	"289",	"290",	"291",	"292",	"293",	"294",	"295",	"296",	"297",	"298",	"299",	"300",	"301",	"302",	"303",	"304",	"305",	"306",	"307",	"308",	"309",	"310",	"311",	"312",	"313",	"314",	"315",	"316",	"317",	"318",	"319",	"320",	"321",	"322",	"323",	"324",	"325",	"326",	"327",	"328",	"329",	"330",	"331",	"332",	"333",	"334",	"335",	"336",	"337",	"338",	"339",	"340",	"341",	"342",	"343",	"344",	"345",	"346",	"347",	"348",	"349",	"350",	"351",	"352",	"353",	"354",	"355",	"356",	"357",	"358",	"359",	"360",	"361",	"362",	"363",	"364",	"365",	"366")
orb_param <- years %>%
  purrr::map_df(palinsol::astro, solution = palinsol::ber78, degree = FALSE)
orb_param <- orb_param %>%
  dplyr::mutate(year = years, .before = 1)
seq<-seq(36.75,71.25,0.5)
lat <- 36.25

insol_tbl <- mid_month_tsl_present %>%
  purrr::map(palinsol::Insol, orbit = orb_param, lat = lat * pi / 180, S0 = 1365) %>%
  magrittr::set_names(names) %>%
  tibble::as_tibble()

## Append the years to the table with the insolation values
insol_tbl <- insol_tbl %>%
  dplyr::mutate(year = years, .before = 1)

## Print table with the first 10 rows
final<-insol_tbl %>%
  dplyr::slice(1:2) %>%
  knitr::kable()

for (i in seq){
  lat <- i
  
  insol_tbl <- mid_month_tsl_present %>%
    purrr::map(palinsol::Insol, orbit = orb_param, lat = lat * pi / 180, S0 = 1365) %>%
    magrittr::set_names(names) %>%
    tibble::as_tibble()
  
  ## Append the years to the table with the insolation values
  insol_tbl <- insol_tbl %>%
    dplyr::mutate(year = years, .before = 1)
  
  ## Print table with the first 10 rows
  file<-insol_tbl %>%
    dplyr::slice(1:2) %>%
    knitr::kable()
  final<-rbind(final,file)}

####solar match
solarH<-read.csv("D://Project/resolution0.5/changesolar/9000/insolationH.csv",header=TRUE)
solarP<-read.csv("D://Project/resolution0.5/changesolar/9000/insolationP.csv",header=TRUE)
solarH<-solarH[,(-1)]
solarP<-solarP[,(-1)]
ratio<-solarH/solarP
ratio[is.na(ratio)]<-0
fwrite(x=ratio,file='D://Project/resolution0.5/changesolar/9000/ratio.csv')



files <- paste("D://Project/resolution0.5/location3676/location", 1:3676, ".csv", sep = "")
solar<-read.csv("D://Project/resolution0.5/changesolar/9000/solardata/ratio.csv",header=TRUE)
solar<-solar[,(-1)]
a<-1:3
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-4:17
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-18:36
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-37:66
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-67:109
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-110:157
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-158:212
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-213:270
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-271:327
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-328:396
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-397:464
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-465:528
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-529:588
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-589:655
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-656:718
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-719:791
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-792:868
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-869:946
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-947:1029
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1030:1107
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1108:1179
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1180:1247
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1248:1318
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1319:1374
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1375:1431
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1432:1490
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1491:1545
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1546:1601
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1602:1660
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1661:1721
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1722:1780
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1781:1837
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1838:1888
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1889:1950
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-1951:2015
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2016:2081
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2082:2145
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2146:2204
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2205:2270
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2271:2344
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2345:2424
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2425:2494
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2495:2562
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2563:2625
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2626:2683
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2684:2748
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2749:2813
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2814:2885
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2886:2944
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-2945:3010
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3011:3078
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3079:3142
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3143:3200
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3201:3251
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3252:3287
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3288:3319
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3320:3349
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3350:3390
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3391:3429
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3430:3465
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3466:3495
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3496:3527
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3528:3557
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3558:3577
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3578:3603
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3604:3619
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3620:3637
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3638:3656
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3657:3666
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
a<-3667:3675
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}
solar<-solar[,(-1)]
solar<-data.frame(solar)
a<-3676
for (i in a){
  basic<-read.csv(files[i],header=FALSE)
  solarpoint<-data.frame(solar[,1])
  basic[,2]<-basic[,2]*solarpoint
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changesolar/location/","location",i,".csv"),col.names = FALSE)
}

##change csv to dat
files <- paste("D://Project/resolution0.5/changesolar/9000/location/location", 1:3676, ".csv", sep = "")
library(multiplex)
for(i in 1:3676){
  a<-read.csv(files[i],header=FALSE)
  fwrite(a, file =paste0("D://Project/resolution0.5/changesolar/9000/locationdat/","location",i,".dat"),col.names = FALSE)
}
###remove txt first line
files <- paste("D://Project/resolution0.5/changesolar/9000/txthavefirstline/Location", 1:3676, ".txt", sep = "")
for (i in 1:3676){
  other<-read.delim(files[i],header=FALSE)
  other<-other[-1,]
  other<-data.frame(other)
  fwrite(x=other,file=paste0("D://Project/resolution0.5/changesolar/9000/output/Location",i,".txt"),col.names=FALSE)
}

##maximum lake temperature
files <- paste("D://Project/resolution0.5/changesolar/9000/output/Location", 1:3676, ".txt", sep = "")
lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(183:213),]
for (i in 2:3676){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(183:213),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
JulT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(214:244),]
for (i in 2:3676){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(214:244),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
AugT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(245:274),]
for (i in 2:3676){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(245:274),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:30),],2,mean)
finalmean<-data.frame(finalmean)
SepT<-finalmean

basic<-cbind(JulT,AugT,SepT)
basic$lakemax<-apply(basic,1,max)
fwrite(x=basic,file='D://Project/resolution0.5/changesolar/9000/lakemax.csv')

###minimum lake temperature
files <- paste("D://Project/resolution0.5/changesolar/9000/output/Location", 1:3676, ".txt", sep = "")
lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(32:60),]
for (i in 2:3676){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(32:60),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:29),],2,mean)
finalmean<-data.frame(finalmean)
FebT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(1:31),]
for (i in 2:3676){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(1:31),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
JanT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(336:366),]
for (i in 2:3676){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(336:366),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
DecT<-finalmean
basic<-cbind(DecT,JanT,FebT)
basic$lakemin<-apply(basic,1,min)
fwrite(x=basic,file='D://Project/resolution0.5/changesolar/9000/lakemin.csv')


##solar radiation
files2 <- paste("D://Project/resolution0.5/changesolar/9000/location/location", 1:3676, ".csv", sep = "")
air<-read.csv(files2[1],header=FALSE)
colnames(air) <-c("a","b","V3","c","d","e")
airT<-air[2]
final<-airT[(183:213),]
for (i in 2:3676){
  air<-read.csv(files2[i],header=FALSE)
  colnames(air) <-c("a","b","V3","c","d","e")
  airT<-air[2]
  airJul<-airT[(183:213),]
  final<-cbind(final,airJul)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
JulS<-finalmean
fwrite(x=JulS,file="D://Project/resolution0.5/changesolar/9000/Julysolar.csv")




###############################################change temperature#####################################
##import nc file
library(ncdf4)
setwd("D://Project/resolution0.5/changeT/EPOCH-2_Mauri_etal_QSR")
nc_data<-nc_open("EPOCH-2.nc")
nc_data
library(raster)
r1<-brick("EPOCH-2.nc",varname = "tanom_djf")
holowinT<- as.data.frame(r1,xy=T)
holowinT<-cbind(holowinT$x,holowinT$y,holowinT$X9000)
colnames(holowinT) <-c("lon","lat","W9000")
r2<-brick("EPOCH-2.nc",varname = "tanom_jja")
holosumT<- as.data.frame(r2,xy=T)
holosumT<-cbind(holosumT$X9000)
colnames(holosumT) <-c("S9000")
holoT<-cbind(holowinT,holosumT)
holoT<-data.frame(holoT)
library(data.table)
tem<-holoT[!is.na(holoT$W9000), ]
fwrite(x=tem, file='D://Project/resolution0.5/changeT/9000/holoT.csv')

tem<-read.csv("D://Project/resolution0.5/changeT/9000/holoT.csv",header=TRUE)
location<-read.csv("D://Project/resolution0.5/3676/3676location.csv",header=TRUE)
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
europe_raster<-raster(xmn=-10,xmx=50,ymn=35,ymx=75,res=0.5,crs='+init=epsg:4326')
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)
locationwgs<-st_as_sf(location, coords=c('lon', 'lat'), crs=4326)
location_raster<-rasterize(as(locationwgs, 'Spatial'),europe_raster)
relation<-extract(tem_raster,as(locationwgs,'Spatial'))
relation<-cbind(locationwgs,relation)
relation<-subset(relation,W9000!="NA")
fwrite(x=relation,file='D://Project/resolution0.5/changeT/9000/tem3.csv')
relation<-read.csv("D://Project/resolution0.5/changeT/9000/tem3.csv",header=TRUE)
library(tidyr)
relation<-relation%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
fwrite(x=relation,file="D://Project/resolution0.5/changeT/9000/tem3.csv")


tem<-read.csv("D://Project/resolution0.5/changeT/9000/holoT.csv",header=TRUE)
location<-read.csv("D://Project/resolution0.5/3676/3676location.csv",header=TRUE)
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
europe_raster<-raster(xmn=-10,xmx=50,ymn=35,ymx=75,res=0.5,crs='+init=epsg:4326')
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)
locationwgs<-st_as_sf(location, coords=c('lon', 'lat'), crs=4326)
location_raster<-rasterize(as(locationwgs, 'Spatial'),europe_raster)
relation<-extract(tem_raster,as(locationwgs,'Spatial'))
relation<-cbind(locationwgs,relation)
relation<-subset(relation,W9000!="NA")
fwrite(x=relation,file='D://Project/resolution0.5/changeT/9000/tem4.csv')
relation<-read.csv("D://Project/resolution0.5/changeT/9000/tem4.csv",header=TRUE)
library(tidyr)
relation<-relation%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
fwrite(x=relation,file="D://Project/resolution0.5/changeT/9000/tem4.csv")

tem<-read.csv("D://Project/resolution0.5/changeT/9000/holoT.csv",header=TRUE)
location<-read.csv("D://Project/resolution0.5/3676/3676location.csv",header=TRUE)
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
europe_raster<-raster(xmn=-10,xmx=50,ymn=35,ymx=75,res=0.5,crs='+init=epsg:4326')
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)
locationwgs<-st_as_sf(location, coords=c('lon', 'lat'), crs=4326)
location_raster<-rasterize(as(locationwgs, 'Spatial'),europe_raster)
relation<-extract(tem_raster,as(locationwgs,'Spatial'))
relation<-cbind(locationwgs,relation)
relation<-subset(relation,W9000!="NA")
fwrite(x=relation,file='D://Project/resolution0.5/changeT/9000/tem2.csv')
relation<-read.csv("D://Project/resolution0.5/changeT/9000/tem2.csv",header=TRUE)
library(tidyr)
relation<-relation%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
fwrite(x=relation,file="D://Project/resolution0.5/changeT/9000/tem2.csv")

tem<-read.csv('D://Project/resolution0.5/changeT/9000/tem3.csv')
b<-(tem$W9000+tem$S9000)/2
a<-(tem$S9000-tem$W9000)/2
t<-c(2:366)
final<-tem$W9000
for(i in t){
  y=a*sin((1/182*i-46/91)*pi)+b
  final<-cbind(final,y)
}
final9000<-as.data.frame(t(final))
fwrite(x=final9000,file='D://Project/resolution0.5/changeT/9000/9000T.csv')


tem<-read.csv('D://Project/resolution0.5/changeT/9000/T/9000T.csv',header=TRUE)
location<-read.csv("D://Project/resolution0.5/changeT/9000/T/tem3.csv",header=TRUE)
seq<-location$﻿no
files <- paste("D://Project/resolution0.5/location3676/location", 1:3676, ".csv", sep = "")
for (i in seq){
  basic<-read.csv(files[i],header=FALSE)
  y<-which(seq==i)
  basic[3]<-tem[,y]+basic[3]
  fwrite(x=basic,file=paste0("D://Project/resolution0.5/changeT/9000/location/","location",y,".csv"),col.names = FALSE)
}

##change csv to dat
files <- paste("D://Project/resolution0.5/changeT/9000/location/location", 1:2483, ".csv", sep = "")
library(multiplex)
for(i in 1:2483){
  a<-read.csv(files[i],header=FALSE)
  fwrite(a, file =paste0("D://Project/resolution0.5/changeT/9000/locationdat/","location",i,".dat"),col.names = FALSE)
}

###remove txt first line
files <- paste("D://Project/resolution0.5/changeT/9000/txthavefirstline/Location", 1:2483, ".txt", sep = "")
for (i in 1:2483){
  other<-read.delim(files[i],header=FALSE)
  other<-other[-1,]
  other<-data.frame(other)
  fwrite(x=other,file=paste0("D://Project/resolution0.5/changeT/9000/output/Location",i,".txt"),col.names=FALSE)
}

##maximum lake temperature
files <- paste("D://Project/resolution0.5/changeT/9000/output/Location", 1:2483, ".txt", sep = "")
lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(183:213),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(183:213),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
JulT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(214:244),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(214:244),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
AugT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(245:274),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(245:274),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:30),],2,mean)
finalmean<-data.frame(finalmean)
SepT<-finalmean
basic<-cbind(JulT,AugT,SepT)
basic$lakemax<-apply(basic,1,max)
fwrite(x=basic,file='D://Project/resolution0.5/changeT/9000/lakemax.csv')

###minimum lake temperature
files <- paste("D://Project/resolution0.5/changeT/9000/output/Location", 1:2483, ".txt", sep = "")
lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(32:60),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(32:60),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:29),],2,mean)
finalmean<-data.frame(finalmean)
FebT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(1:31),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(1:31),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
JanT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(336:366),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(336:366),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
DecT<-finalmean
basic<-cbind(DecT,JanT,FebT)
basic$lakemin<-apply(basic,1,min)
fwrite(x=basic,file='D://Project/resolution0.5/changeT/9000/lakemin.csv')


##maximum air temperature
files2 <- paste("D://Project/resolution0.5/changeT/9000/location/Location", 1:2483, ".csv", sep = "")
air<-read.csv(files2[1],header=FALSE)
colnames(air) <-c("a","b","V3","c","d","e")
airT<-air[3]
final<-airT[(245:274),]
for (i in 2:2483){
  air<-read.csv(files2[i],header=FALSE)
  colnames(air) <-c("a","b","V3","c","d","e")
  airT<-air[3]
  airJul<-airT[(245:274),]
  final<-cbind(final,airJul)
}
finalmean<-apply(final[(1:30),],2,mean)
finalmean<-data.frame(finalmean)
SepT<-finalmean

air<-read.csv(files2[1],header=FALSE)
colnames(air) <-c("a","b","V3","c","d","e")
airT<-air[3]
final<-airT[(214:244),]
for (i in 2:2483){
  air<-read.csv(files2[i],header=FALSE)
  colnames(air) <-c("a","b","V3","c","d","e")
  airT<-air[3]
  airJul<-airT[(214:244),]
  final<-cbind(final,airJul)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
AugT<-finalmean

air<-read.csv(files2[1],header=FALSE)
colnames(air) <-c("a","b","V3","c","d","e")
airT<-air[3]
final<-airT[(183:213),]
for (i in 2:2483){
  air<-read.csv(files2[i],header=FALSE)
  colnames(air) <-c("a","b","V3","c","d","e")
  airT<-air[3]
  airJul<-airT[(183:213),]
  final<-cbind(final,airJul)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
JulT<-finalmean
basic<-cbind(JulT,AugT,SepT)
basic$airmax<-apply(basic,1,max)
fwrite(x=basic,file="D://Project/resolution0.5/changeT/9000/airmax.csv")

##minimum air temperature
files2 <- paste("D://Project/resolution0.5/changeT/9000/location/Location", 1:2483, ".csv", sep = "")
air<-read.csv(files2[1],header=FALSE)
colnames(air) <-c("a","b","V3","c","d","e")
airT<-air[3]
final<-airT[(32:69),]
for (i in 2:2483){
  air<-read.csv(files2[i],header=FALSE)
  colnames(air) <-c("a","b","V3","c","d","e")
  airT<-air[3]
  airJul<-airT[(32:69),]
  final<-cbind(final,airJul)
}
finalmean<-apply(final[(1:29),],2,mean)
finalmean<-data.frame(finalmean)
FebT<-finalmean

air<-read.csv(files2[1],header=FALSE)
colnames(air) <-c("a","b","V3","c","d","e")
airT<-air[3]
final<-airT[(1:31),]
for (i in 2:2483){
  air<-read.csv(files2[i],header=FALSE)
  colnames(air) <-c("a","b","V3","c","d","e")
  airT<-air[3]
  airJul<-airT[(1:31),]
  final<-cbind(final,airJul)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
JanT<-finalmean

air<-read.csv(files2[1],header=FALSE)
colnames(air) <-c("a","b","V3","c","d","e")
airT<-air[3]
final<-airT[(336:366),]
for (i in 2:2483){
  air<-read.csv(files2[i],header=FALSE)
  colnames(air) <-c("a","b","V3","c","d","e")
  airT<-air[3]
  airJul<-airT[(336:366),]
  final<-cbind(final,airJul)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
DecT<-finalmean
basic<-cbind(DecT,JanT,FebT)
basic$airmin<-apply(basic,1,min)
fwrite(x=basic,file="D://Project/resolution0.5/changeT/9000/airmin.csv")


tem<-read.csv("D://Project/resolution0.5/changeT/9000/T/tem3.csv",header=TRUE)
seq<-tem$﻿no
location<-read.csv("D://Project/resolution0.5/origin3676/difference.csv")
location1<-location[(seq),]
library(data.table)
fwrite(x=location1,file='D://Project/resolution0.5/changeT/9000/2483.csv')

#################################change solar and temperature####################################
files <- paste("D://Project/resolution0.5/changeT/9000/location/location", 1:2483, ".csv", sep = "")
files2 <- paste("D://Project/resolution0.5/changesolar/9000/location/location", 1:3676, ".csv", sep = "")
location<-read.csv("D://Project/resolution0.5/changeT/9000/T/tem3.csv",header=TRUE)
seq<-location$﻿no
for (i in seq){
  y<-which(seq==i)
  tem<-read.csv(files[y],header=FALSE)
  solar<-read.csv(files2[i],header=FALSE)
  tem[2]<-solar[2]
  fwrite(tem, file =paste0("D://Project/resolution0.5/changesolarT/9000/location/","location",y,".csv"),col.names = FALSE)
}

##change csv to dat
files <- paste("D://Project/resolution0.5/changesolarT/9000/location/location", 1:2483, ".csv", sep = "")
library(multiplex)
for(i in 1:2483){
  a<-read.csv(files[i],header=FALSE)
  fwrite(a, file =paste0("D://Project/resolution0.5/changesolarT/9000/locationdat/","location",i,".dat"),col.names = FALSE)
}

###remove txt first line
files <- paste("D://Project/resolution0.5/changesolarT/9000/txthavefirstline/Location", 1:2483, ".txt", sep = "")
for (i in 1:2483){
  other<-read.delim(files[i],header=FALSE)
  other<-other[-1,]
  other<-data.frame(other)
  fwrite(x=other,file=paste0("D://Project/resolution0.5/changesolarT/9000/output/Location",i,".txt"),col.names=FALSE)
}

##maximum lake temperature
files <- paste("D://Project/resolution0.5/changesolarT/9000/output/Location", 1:2483, ".txt", sep = "")
lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(183:213),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(183:213),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
JulT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(214:244),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(214:244),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
AugT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(245:274),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(245:274),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:30),],2,mean)
finalmean<-data.frame(finalmean)
SepT<-finalmean
basic<-cbind(JulT,AugT,SepT)
basic$lakemax<-apply(basic,1,max)
fwrite(x=basic,file='D://Project/resolution0.5/changesolarT/9000/lakemax.csv')

###minimum lake temperature
files <- paste("D://Project/resolution0.5/changesolarT/9000/output/Location", 1:2483, ".txt", sep = "")
lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(32:60),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(32:60),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:29),],2,mean)
finalmean<-data.frame(finalmean)
FebT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(1:31),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(1:31),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
JanT<-finalmean

lake<-read.table(files[1],header=FALSE)
lake<-lake[-1,]
lake<-lake[4]
lake$V4<-as.numeric(lake$V4)
final<-lake[(336:366),]
for (i in 2:2483){
  lake1<-read.table(files[i],header=FALSE)
  lake1<-lake1[-1,]
  lake1<-lake1[4]
  lake1$V4<-as.numeric(lake1$V4)
  lake1<-lake1[(336:366),]
  final<-cbind(final,lake1)
}
finalmean<-apply(final[(1:31),],2,mean)
finalmean<-data.frame(finalmean)
DecT<-finalmean
basic<-cbind(DecT,JanT,FebT)
basic$lakemin<-apply(basic,1,min)
fwrite(x=basic,file='D://Project/resolution0.5/changesolarT/9000/lakemin.csv')

####################################insolation######################################
temH<-read.csv("D://Project/resolution0.5/changesolar/9000/solardata/insolationH.csv")
temP<-read.csv("D://Project/resolution0.5/changesolar/9000/solardata/insolationP.csv")
diff<-temH-temP
diff<-diff[(183:213),]
diffmean<-apply(diff[(1:31),],2,mean)
diffmean<-data.frame(diffmean)
seq<-seq(71.75,36.25,-0.5)
seq<-data.frame(seq)
diffmean$latitude<-seq

temH<-read.csv("D://Project/resolution0.5/changesolar/9000/solardata/insolationH.csv")
temP<-read.csv("D://Project/resolution0.5/changesolar/9000/solardata/insolationP.csv")
diff2<-temH-temP
diff2<-diff2[(183:213),]
diffmean2<-apply(diff2[(1:31),],2,mean)
diffmean2<-data.frame(diffmean2)
diffmean$new9<-diffmean2

diffmean<-read.csv('D://Project/resolution0.5/changesolar/insolationdiff.csv')
ggplot(diffmean)+
geom_line(aes(x=latitude,y=diffmean),color='brown1',lwd=1.2)+
geom_line(aes(x=latitude,y=new9),color='cornflowerblue',lwd=1.2)+
labs(x='Laittude',y='Insolation difference (W/m^2)')

ggplot(diffmean)+
  geom_line(aes(x=latitude,y=diffmean,color=Year),lwd=1.2)+
  labs(x='Laittude',y='Insolation difference (W/m^2)')
