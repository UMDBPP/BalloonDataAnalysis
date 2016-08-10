# This script merges datasets, specifically from IRENE and LINK-TLM, and creates two new data frames "outer_join" and "inner_join"
# Requires package "measurements" to be installed (install.packages("measurements"))

launch_number <- "NS57"
tlm_data <- parse_tlm_data(paste(launch_number, "/", launch_number, "_parsedPackets", ".txt", sep = ""))
irene_data <- parse_irene_data(paste(launch_number, "/", launch_number, "LaunchData", ".txt", sep = ""))

################################################################################

#install.packages("measurements")
require(measurements)

# join tables (outer join with POSIX timestamp as key)
outer_join <- merge(x = tlm_data, y = irene_data, by = c("Timestamp"), all = TRUE)

# make a copy of joined data that we will now merge (inner join)
inner_join <- outer_join

# add IRENE data to LINK-TLM rows
# TODO find a better way to interpolate data than "nearest past neighbor"
while(anyNA(inner_join$Counts_Per_Minute))
{
    inner_join$Counts_Per_Minute[is.na(inner_join$Counts_Per_Minute)] = inner_join$Counts_Per_Minute[which(is.na(inner_join$Counts_Per_Minute)) - 1]
}

# remove rows not belonging to LINK-TLM and write to CSV
inner_join <- subset(inner_join,!(is.na(Callsign)))
write.csv(inner_join, paste(launch_number, "/", launch_number, "_inner_join", ".txt", sep = ""), row.names=FALSE)
