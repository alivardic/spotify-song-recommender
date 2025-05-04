# Dataset 
df_combined <- as.data.frame(read_csv("df_combined.csv"))

# Please use this search code I provided (Or any other method of your choice) to 
# check if a songs or artists you want to include in your reccomended playlist are 
# in the dataset. When you find a match, I need you to send me the song name, the 
# artist name, and the song’s ID number exactly as they appear in the dataset. 
# I’m using this information to build our evaluation spreadsheets, and it won’t 
# work properly if the songs you select aren’t in the database or if I don’t have 
# the correct ID. Let me know if you have any other questions!

# I need 5 - 10 songs that you genuinely enjoy (preferably 10)

# It’s really important that you genuinely enjoy the songs you select. This entire 
# evaluation will be based on how much you like the model’s recommendations. If you 
# choose songs you don’t actually like, the results won’t be meaningful, and the 
# evaluation won’t work as intended.


# Search Dataset Via Song Name (Capitalization will vary)
df_combined[df_combined$track_name == "familiar", ]
which(df_combined$track_name == "[Song Name]")


# Search Dataset Via Artist Name (Capitalization will vary)
df_combined[df_combined$artists == "Hoobastank", ]
which(df_combined$artists == "[Artist Name]")


# Search Dataset Via ID 
df_combined[df_combined$...1 == 5103, ]
which(df_combined$artists == "[Artist Name]")
