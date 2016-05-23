#Automated Re-Install of Packages

#DO FIRST PART BEFORE UPDAING r VERSION!!!

----

# gets the library trees within which packages are looked for
.libPaths()

# change it to your own dir
myPath = c("C:/Users/GRA/Documents/R/win-library/3.3") 

#Get currently installed packages
package_df <- as.data.frame(installed.packages(myPath))  

#saves as data frame
save(package_df, file = "C:/Users/GRA/Desktop/Misc/R Working Directory/Other/misc/package_mgmt/packages_24MAY2016.rda")

------

#update R, do this in R console
library(installr)
updateR()

------  
  
#Once R is updated, load the dataframe  
load("C:/Users/GRA/Desktop/Misc/R Working Directory/Other/misc/package_mgmt/packages_23MAY2016.rda")

#makes it a list
package_list <- as.character(package_df$Package)

#Re-install all installed packages
install.packages(package_list)
