### Foul Play: Examining NBA Player Behavior in Response to Foul Trouble
This project performs a statistical analysis of players’ defensive tendencies to determine whether foul trouble affects the way players defend. Distance to the closest offensive (opposing team) player was used as the metric for evaluating defensive aggressiveness. The data used to carry out this analysis was of two types: real-time player movement data and play by play data. A large portion of this project was devoted to data cleaning and manipulation, as well as the alignment of the two data sets. Particularly in the player-movement data, there appear to be many errors and inconsistencies, likely due to the inaccuracy and volatility of the player tracking cameras in the arenas. The statistical analysis in the end shows several logical trends, but does not yield promising results on the effect of foul trouble on defensive play. We do find a significant effect of distance on the likelihood of a player committing a foul, and further, there does appear to be a positive relationship between a player’s number of fouls and his distance from the player he is guarding, controlling for other factors such as score differential, shot clock time remaining, game time remaining, and position. Three different measures of foul trouble are used as predictors for defensive player distance to the closest offensive player: number of fouls, squared number of fouls, and likelihood of fouling out in the game (as determined by a Markov model I created). Because none of these measures show a significant effect on defensive play, we can conclude either that players are not modifying their play in response to foul trouble, that more nuanced models of foul trouble are required to capture the situations in which players do adapt, or that the tracking data does not provide the right kind of information to measure the nature of players’ responses.

The file "poster.pdf" is a digital version of the poster that I made for my final presentation.

### foul_trouble_model.R
Contains the function that creates the Markov model for foul trouble mentioned above (this function does not take an input but requires data being loaded into the working environment beforehand).

### Data
This data was taken from @sealneaward's repo. The files were unzipped into JSON files, and then parsed using the "sportvu_convert_json" function in data_parsing_helpers.R and saved as R data files.

### data_parsing_helpers.R
Contains many functions for various purposes, including:
* **sportvu_convert_json:** parses JSON files and converts them into dataframes
* **get_pbp:** grabs play by play data from nba.com
* **travelDist and velocity:** calculate a player's distance traveled and velocity between two data points, respectively
* **player_dist_matrix:** creates a matrix of all distances between the players on the court (and the ball) at a point in time
Many of these functions were adapted from @rajshah4's NBA_SportVuNBA_SportVu/_functions.R.

### process_data.R
* Processes the data, sorting it by quarter and time remaining and removing duplicate rows. 
* Adds several new columns, including the ID's of all players on the court, whether a score change occurred on each possession, when the ball crossed half court, and the identity of the team on offense. 
* Applies a number of filters, keeping rows only where the ball is in the frontcourt, and where the shot clock values (and times that it is reset) match those in the official play by play data. 
* Fixes various inconsistencies in the data, primarily the aforementioned shot clock irregularities, in addition to players occasionally being tracked while not currently in the game.
