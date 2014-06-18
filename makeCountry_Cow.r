#put SSD as south Sudan
countries<-read.csv("lookup.countries.csv")
country.lpj<-read.csv("ListCountries.csv",sep=";")
temp_country_name<-vector()
temp_lpj_code<-vector()
temp_iso2_code<-vector()
temp_iso3_code<-vector()
k<-0

for(i in 1:length(countries$country))
{
	
	search.text<-gsub(" ","_", as.character(countries$country[i]))
	 tt<-which(country.lpj$name==search.text)
	 if(length(tt)!=0)
	 {
		 k<-k+1
		 temp_country_name[k]<-search.text
		 temp_iso2_code[k]<-as.character(countries$ISO2[i])
		 temp_iso3_code[k]<-as.character(countries$ISO3[i])
		 temp_lpj_code[k]<-as.character(country.lpj$cow[tt])
	 }else
	 {
		 cat("can not find",as.character(countries$country[i]),"in LPJmL cow file. \n")
	}
}

country_cow<-data.frame(name=temp_country_name,
							cow=temp_lpj_code,
							iso2=temp_iso2_code,
							iso3=temp_iso3_code)



write.csv(file="country_cow.csv",country_cow)
