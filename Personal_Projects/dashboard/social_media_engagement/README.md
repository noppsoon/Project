# Social Media Engagement Analysis

An exploratory analysis of 12,000 simulated social media posts using Python and Looker Studio. The project examines engagement across platforms, content topics, message features, audiences, posting times and campaigns.

[View the interactive dashboard](https://datastudio.google.com/s/jf3dA_fncPw)

## Project Objective

Explore which combinations of content and delivery factors are associated with higher engagement, and assess audience sentiment, toxicity, buzz change and engagement growth.

## Tools and Approach

- **Python and pandas:** data preparation and exploratory analysis.
- **Looker Studio:** interactive dashboard development.
- **Median engagement rate:** used to compare typical performance because post-level rates contain extreme values.
- **Impressions and post counts:** provide context when interpreting engagement rates.
- **Separate hashtag, keyword and mention tables:** allow analysis of individual terms while retaining the original post-level data for other comparisons.

Engagement rate is calculated as:

`(likes + comments + shares) / impressions`

## Dashboard Pages

1. **Engagement Overview:** overall engagement, impressions, platform comparisons and exceptional engagement rates.
2. **Content and Platform Performance:** topics and hashtags across platforms.
3. **Message Features:** keywords, mentions and text length.
4. **Location and Language:** content performance within geographic and post-language segments.
5. **Posting Time:** monthly patterns and engagement by day and hour.
6. **Campaign Performance:** comparisons by brand, product, campaign and campaign phase.
7. **Audience Response:** sentiment, toxicity, buzz change and user engagement growth.

## Key Findings

- Hashtag differences were small overall. Although `#travel` had the highest overall median engagement rate, no hashtag consistently performed best across all months.
- Hourly median engagement was highest at 11:00, followed by 17:00 and 00:00, but the differences were relatively small.
- Combining factors revealed differences that were less visible in overall comparisons. Rankings therefore need to be interpreted within their platform and content context.

## Interpretation and Limitations

The dataset is machine-generated. Findings illustrate an analytical workflow and should not be treated as evidence of real-world campaign effectiveness.

Observed differences describe associations rather than causal effects. A high engagement rate also requires checking its impressions denominator, because low impressions can produce unusually high rates.

[Dataset source](https://www.kaggle.com/datasets/subashmaster0411/social-media-engagement-dataset)
