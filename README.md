# Online Images Amplify Gender Bias
This github contains all of the replication materials for Guilbeault et al. (2023) "Online Images Amplify Gender Bias"

This git contains:<br>
-An r script for replicating all main and supplementary analyses from the raw metadata ("master script.R").<br>
-The raw image data collected and coded from Google and Wikipedia (see below for details) <br>
-The raw data for all experiments (see below for details) <br>
-Python code for training a skipgram model from scratch ("word2vec_training.ipynb"). <br>
-The word2vec model we retained on a recent sample of online news ("word2vec-retrained.model"). <br>
-A python script which demonstrates how to extract a gender dimension from a word embedding model and position social categories along this dimension (applied to our retrained word2vec model) ("gender_parity_retrained_word2vec.ipynb"). <br>

<br>
Notes:<br>
<br>
1.0. The datasets used in this study are too large to upload as a csv to this git repository, so they can be accessed and downloaded at the following google drive links (you will need to update the filepaths in the replication r script so that it downloads data from your local computer):

Main Data (Google & Wikipedia): https://drive.google.com/file/d/1PE0_NcTcbvDoi0ePxemacFuZ7w4vH9lD/view?usp=sharing <br>
Raw Image Data (Google & Wikipedia): https://compsyn.s3.ca-central-1.amazonaws.com/Guilbeault_etal_Online_Images_Amplify_Gender_Bias_raw_faces.zip (note: "g/" = Google images; "w/" = Wikipedia images)  <br>
Main Data (Experiment): https://drive.google.com/file/d/1oW03dmiYtWGijcvBwPRjpCC_lXClRppD/view?usp=sharing <br>
IP Replication Data (Google): https://drive.google.com/file/d/18Yeb32hlihoD8ptFf9srFMnT3vcDeYmN/view?usp=sharing <br>
Uncropped Data (Google): https://drive.google.com/file/d/1dgpKDnuSvdNbwGGQI4I_bJWjIDBDchHT/view?usp=share_link <br>
IMDb Data (from the IMDb-Wiki dataset): https://drive.google.com/file/d/1X3KNxXACfV76WbiWLmkhNfE8ka7Ik9kw/view?usp=sharing <br>
Wiki Data (from the IMDb-Wiki dataset): https://drive.google.com/file/d/1ddx3-JNrFdRHnSLZ--e-8d1zEBjcl4SH/view?usp=share_link <br>
OpenCV classifications of Google Data (cropped): https://drive.google.com/file/d/1wx9Zoh_Tr6HPSfVd2gPGIlEtzGVS4sp6/view?usp=share_link <br>
OpenCV classifications of Google Data (uncropped): https://drive.google.com/file/d/1wpn1Vk5tFOfw8GYri8-V7jw_Go59rje8/view?usp=share_link <br>
Image Sources Tagged (Experiment): https://drive.google.com/file/d/1nh3ft7t0c2_tUpYXazsknia99cnVi94E/view?usp=share_link <br>
Image Sources Tagged Stats (Experiment): https://drive.google.com/file/d/1wbyKUm6IarsDJ4fnpgJR3fpY55ylBgut/view?usp=share_link <br>
Supplementary Validation Task Data: https://drive.google.com/file/d/1B513crVrZfosR7EngYQ7Mdh0HZn040Op/view?usp=share_link <br>
Raw syn1neg vectors for our retrained skipgram model: https://drive.google.com/file/d/1tZOgM519h8hPcxWyuhAiMZxZfOLA4DBj/view?usp=sharing <br>
Raw wv.vectors for our retrained skipgram model: https://drive.google.com/file/d/1ZsNfK9dsXTaFkPVI_fJXwJHkJv4fz_qV/view?usp=sharing <br>
<br>
1.1. The data associated with the IMDb-Wiki Dataset is from the following paper:

Rothe, Rasmus, Radu Timofte, and Luc Van Gool. “Deep Expectation of Real and Apparent Age from a Single Image Without Facial Landmarks.” International Journal of Computer Vision 126, no. 2 (April 1, 2018): 144–57. https://doi.org/10.1007/s11263-016-0940-3.

1.2. The data associated with the Wikipedia-based Image Text Dataset is from the following paper:

Srinivasan, Krishna, Karthik Raman, Jiecao Chen, Michael Bendersky, and Marc Najork. “WIT: Wikipedia-Based Image Text Dataset for Multimodal Multilingual Machine Learning.” In Proceedings of the 44th International ACM SIGIR Conference on Research and Development in Information Retrieval, 2443–49. New York, NY, USA: Association for Computing Machinery, 2021. https://doi.org/10.1145/3404835.3463257.

The WIT data did not originally contain the age and gender classifications. Our dataset includes the gender and social category classifications for images from this data, which we collected using human crowdsourcing from Mturk (see full paper for methodological details).
