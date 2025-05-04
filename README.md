
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Song Recommendation Algorithms Using K-Means, KNN, and Spotify Echo Nest

## Project Overview

Our goal was to build a music recommendation system using Spotify’s
now-deprecated Echo Nest audio features. Specifically, we wanted to
develop a predictive model that recommends five songs a user might enjoy
based on one input track they already like.

This project explores unsupervised learning, practical modeling,
subjective evaluation, and the creative limitations of working with
real-world audio data.

------------------------------------------------------------------------

## 📊 Dataset: Spotify Echo Nest Features

The Echo Nest began as a project at the MIT Media Lab focused on music
identification, playlist generation, and audio analysis using deep
learning. It was acquired by Spotify in 2014 and has since powered much
of their personalized recommendation system.

Although Spotify deprecated access to Echo Nest features via API in late
2024/early 2025, historical datasets remain accessible thanks to prior
scraping efforts by data analysts. This project relied on those archived
datasets.

Our dataset includes the following numeric/audio-based features:

- `duration_ms`: Song length in milliseconds  
- `popularity`: Score (0–100) based on user engagement  
- `danceability`: How well-suited a song is for dancing (0–1)  
- `energy`: Intensity and activity level of a song (0–1)  
- `key`: Musical key encoded as 0–11  
- `loudness`: Average decibel level  
- `mode`: 1 = major, 0 = minor  
- `speechiness`: Presence of spoken word content (0–1)  
- `acousticness`: Likelihood of acoustic elements (0–1)  
- `instrumentalness`: Probability the track is instrumental (0–1)  
- `liveness`: Likelihood of being a live performance (0–1)  
- `valence`: Positivity or “cheerfulness” of the track (0–1)  
- `tempo`: Beats per minute (BPM)

> Note: These features are proprietary. We do not know the exact
> formulas Spotify used to calculate them, and as such, we had no way to
> verify the accuracy of the datatsets these were pulled from.

------------------------------------------------------------------------

## Models: Simplicity First

Initially, our plan was straightforward: use K-Nearest Neighbors (KNN)
to identify songs statistically similar based on Spotify’s Echo Nest
features. We were fortunate that our variables showed no significant
correlations (none exceeded a correlation of 0.8), meaning minimal
preprocessing was needed. This allowed us to rapidly prototype a simple
baseline model – just three lines of code – to quickly gauge
effectiveness.

Our initial, simple KNN model performed surprisingly well, meeting
expectations despite its simplicity. Curious if complexity would yield
better results, we incrementally tested more advanced methods available
to us (excluding algorithms outside of our class toolkit like neural
networks, DBSCAN, or topic modeling).

We ended up with three unsupervised models based on combinations of
K-Nearest Neighbors (KNN), K-Means clustering, and Principal Component
Analysis (PCA):

- `Model 1:` KNN
- `Model 2:` K-Means clustering → KNN within clusters  
- `Model 3:` K-Means → PCA → KNN

> For full technical documentation — including how we selected optimal
> cluster counts, KNN tuning, and cluster evaluation — please see our
> Model Report.

------------------------------------------------------------------------

## Evaluation: Manual & Subjective

As we had **no outcome variable**, we couldn’t use traditional metrics
like accuracy or confusion matrices. Instead, we used a
human-in-the-loop evaluation:

- Each team member submitted 10 songs they liked.
- Each model recommended 5 songs per input (50 total per model).
- Members marked each recommended track as “liked” or “disliked” (1 or
  0).
- No criteria were enforced — feedback was based solely on personal
  taste.

### 📊 Results

| Model   | Avg. Accuracy | Notes                  |
|---------|---------------|------------------------|
| Model 2 | 61.5%         | The Fan Favorite       |
| Model 1 | 59%           | Simple, But Efficient  |
| Model 3 | 59%           | Overfit, underwhelming |

> Due to subjective goal of our project, this approach aligned with our
> original project goal: building a recommendation model focused on user
> enjoyment rather than purely statistical or acoustic similarity. In
> practice, users don’t typically care about numeric similarity—they
> care whether they actually enjoy the music suggested to them. Thus, we
> prioritized real-world usefulness over abstract accuracy metrics,
> allowing annotators complete freedom in their judgments.

> For full documentation — including how we queried each annotators
> playlists, annotator responses, and summary statistics — please see
> our Annotation Report.

------------------------------------------------------------------------

## 🛠️ App: [ShinyApp.io Interactive Song Finder](#)

Based on the evaluation results, we continued forward with our final
goal: Building a Shiny App that people can use to find new music with
our algorithm. We integrated Model 2 into the app due to its performance
in the manual evaluation. In the Shiny app, users can:

- Input a favorite song  
- View 5 closest matches  
- Explore audio distances  
- Click Spotify links to listen

> Note: Shiny link —
> \[daniel-tafmizi.shinyapps.io/final_proj_shinyapp/\].

------------------------------------------------------------------------

## ⚠️ Limitations

- **Subjectivity**: Music taste varies by person and context.  
- **Feature Blindness**: Similar tempo/energy doesn’t always = similar
  vibe.  
- **Metadata Gap**: We lacked genre, lyrics, or emotional descriptors.  
- **API Loss**: Spotify shut down access to these features during our
  project.  
- **Proprietary Calculations**: We cannot verify how Echo Nest metrics
  were derived.

> *Lesson learned*: Always archive your data. APIs disappear, and
> documentation fades.

------------------------------------------------------------------------

## Key Takeaways

- **Unsupervised learning** can still be predictive — just not in
  traditional ways.
- **Simple models** can outperform complex ones, especially in creative
  tasks.
- **Subjective validation** is messy but often necessary in real world
  application.
- **Some Factors will always be unpredictable** and you will never know
  everything that impacts subjective ratings
- **Data limitations** often come from the outside — platforms, APIs, or
  undocumented pipelines.

------------------------------------------------------------------------

## 👥 The Team

- **Alina Hagen** — Model Design and Evaluation
- **Daniel Tafmizi** — Model and App Design  
- **Alexander Crowell** — Model Evaluation and Visualization  
- **Anthony Venson** — Annotation and Debugging
