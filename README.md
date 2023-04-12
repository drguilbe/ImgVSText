# Online Gender Bias is Stronger in Images than Text
This github contains all of the replication materials for Guilbeault et al. (2023) "Online Gender Bias is Stronger in Images than Text"

This git contains:
-An r script for replicating all main and supplementary analyses from the raw data.
-The raw data for all experiments

Notes:

_1.0. The datasets used in this study are too large to upload as a csv to this git repository, so they can be accessed and downloaded at the following google drive links (you will need to update the filepaths in the replication r script so that it downloads data from your local computer):

Main Data (Google & Wikipedia): 
IP Replication Data (Google): 
Supplementary Validation Task Data: https://drive.google.com/file/d/12hJxTtMcA9fJix8lBZGWB4xBM7_-BiJR/view?usp=sharing
Uncropped Data (Google):
IMDb Data (from the IMDb-Wiki dataset): https://drive.google.com/file/d/1UzCdTNr5bXff7o0Gwaq1V0WI81Qih77n/view?usp=sharing
Wiki Data (from the IMDb-Wiki dataset): https://drive.google.com/file/d/1EqlRkyLsCW86DaR3U5MA3dcwqM5LODxH/view?usp=sharing
Google Trends Search Frequency Data: https://drive.google.com/file/d/15u4lZ3oB7qXExwk-5I2n305G-IUGfBX2/view?usp=sharing


1.1. The data associated with the IMDb-Wiki Dataset is from the following paper:

Rothe, Rasmus, Radu Timofte, and Luc Van Gool. “Deep Expectation of Real and Apparent Age from a Single Image Without Facial Landmarks.” International Journal of Computer Vision 126, no. 2 (April 1, 2018): 144–57. https://doi.org/10.1007/s11263-016-0940-3.

1.2. The data associated with the Wikipedia-based Image Text Dataset is from the following paper:

Srinivasan, Krishna, Karthik Raman, Jiecao Chen, Michael Bendersky, and Marc Najork. “WIT: Wikipedia-Based Image Text Dataset for Multimodal Multilingual Machine Learning.” In Proceedings of the 44th International ACM SIGIR Conference on Research and Development in Information Retrieval, 2443–49. New York, NY, USA: Association for Computing Machinery, 2021. https://doi.org/10.1145/3404835.3463257.

The WIT data did not originally contain the age and gender classifications. Our dataset includes the gender and social category classifications for images from this data, which we collected using human crowdsourcing from Mturk (see full paper for methodological details).
