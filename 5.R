#Project 4

good_features <- filter(missing_values, missing_pct<0.25)
vars <- good_features$feature[str_detect(good_features$feature,'num_')]
cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp <- cor_tmp %>% select(one_of(c(vars,"logerror")))

common <- intersect(colnames(cor_tmp), good_features$feature)
transactions01 <- transactions %>%
  select(id_parcel, logerror, abs_logerror)
cor_tmp01 <- transactions01 %>% left_join(cor_tmp[common], by="id_parcel")
drops <- c("id_parcel", "fips", "latitude", "longitude", "zoning_landuse_county", "zoning_property", 
           "rawcensustractandblock", "region_city", "region_zip", "censustractandblock", "tax_year", 
           "tax_building", "tax_land")
cor_tmp01 <- cor_tmp01[ , !(names(cor_tmp01) %in% drops)]
corrplot(cor(cor_tmp01, use="complete.obs"),type="lower")
cor(cor_tmp01, use="complete.obs")
drops <- c("num_bath", "num_bathroom_calc", "area_live_finished", "tax_property")
cor_tmp01 <- cor_tmp01[,!names(cor_tmp01) %in% drops]
str(cor_tmp01)

## Convert region_county and zoning_landuse to a factor from an integer to indicate that these 
## variables should be treated as a categorical variable
cor_tmp01$region_county=factor(cor_tmp01$region_county)
cor_tmp01$zoning_landuse=factor(cor_tmp01$zoning_landuse)
str(cor_tmp01)

fit1=lm(logerror~.-abs_logerror, data=cor_tmp01)
summary(fit1)
library(leaps)
fit1a=regsubsets(logerror~.-abs_logerror,data=cor_tmp01)
summary(fit1a)

fit2=lm(abs_logerror~.-logerror, data=cor_tmp01)
summary(fit2)
fit2a=regsubsets(abs_logerror~.-logerror,data=cor_tmp01)
summary(fit2a)

# logerror = log(Zestimate) - log(Saleprice)
# If logerror is negative, that means that the Zestimate is underestimating the Saleprice; a positive logerror
# means that the Zestimate is overestimating the Saleprice. 
# A small absolute logerror value means that the log(Zestimate) is close to log(Saleprice), meaning 
# the Zestimate predictions are pretty accurate.
# The linear regression of logerror shows that area_total_calc, zoning_landuse47, tax_total, tax_delinquency, 
# zoning_landuse263, zoning_landuse260, and zoning_landuse269 are all signifianct variables, with p values
# less than 0.05. By using regsubset, we can find the best subsets, forward selection and backwards elimination
# depending on which approach is more appropriate. The regsubset shows significance in area+total_calc, zoning_landuse47,
# tax_total, tax_delinquency, zoning_landuse247, zoning_landuse248, zoning_landuse263, zoning_landinguse266,
# region_county2061.
# The linear regression for absolute logerror shows significance in absolute log error, num_bedroom, 
# area_total_calc, area_lot, all of the zoning_landuse variables as well as region_county variables, build_year,
# tax_total, and tax_delinquency. The regsubset function shows signficance in num_bedroom, area_total_calc, 
# zoning_landuse47, zoning_landuse261, zoning_landuse266, zoning_landuse269, region_county3101, build_year,
# tax_deliquency.
# We assume that the Zestimate is a good and accurate model which means that the logerror should be an independent 
# variable. However, through the regression analysis, it shows that Zestimate is not always accurate and is, in fact,
# affected by additional variables (as seen above). Since the zoning landuse variable is actually a factor, this shows
# that there is a high correlation between the absolute logerror in certain zones. 
