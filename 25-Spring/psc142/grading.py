from docx import Document
import ollama
import os
import time
import csv
from tqdm import tqdm
from datetime import datetime
import re

# ===== CONFIGURATION =====
RUBRIC_PATH = r"/run/media/mcarmo/662229CC2229A253/RProjects/phd/25-Spring/psc142/instructions.docx"
ESSAYS_FOLDER = r"/run/media/mcarmo/662229CC2229A253/RProjects/phd/25-Spring/psc142/essays/"
DELAY_BETWEEN_ESSAYS = 3  # Seconds between requests
CSV_GRADEBOOK = os.path.join(ESSAYS_FOLDER, "gradebook.csv")
# =========================

def extract_docx_text(file_path):
    """Extract text from a DOCX file"""
    try:
        doc = Document(file_path)
        return "\n".join([para.text for para in doc.paragraphs if para.text.strip()])
    except Exception as e:
        print(f"\nError reading {file_path}: {str(e)}")
        return None

def grade_essay(rubric_text, essay_text):
    """Generate structured grading output based on rubric"""
    prompt = f"""
    [ROLE]
    You are a university professor grading student essays against a specific rubric.
    Each of the rubric's topic is worth a certain amount of points specified in parenthesis. Your task is to evaluate each of those criteria following
    the description of what the student should present in their paper. 

    The main general instruction for the students was this:
    "Your assignment is to read the media article, consider the message it delivers, 
    and evaluate the empirical accuracy of that message. (If there are multiple messages in the article, 
    select one, or at most two, to focus on.) You will evaluate the media article’s message using two 
    empirical articles published in peer-reviewed journals in the past 5 years (2020-2025). 
    If you want, and it’s helpful, you can also use the course’s assigned readings (textbook and/or articles) 
    as source materials for your paper."

    Provide detailed assessment ONLY in the required format.
    
    [REQUIRED OUTPUT FORMAT]
    ### ESSAY GRADING REPORT
    
    **Rubric Criteria Assessment:**
    For each criterion below, provide:
    1. Score (X/Y points)
    2. Specific evidence from the text
    3. Explanation of how it meets/falls short of expectations
    
    **Strengths:**
    - List specific strengths with examples
    
    **Areas for Improvement:**
    - List concrete suggestions with examples
    
    **Overall Score:** [X]/[Y]
    **Final Comments:** [Brief summary]
    
    [RUBRIC]
    INTRODUCTION (5 points) 
The student identifies the selected focal topic for the paper. What is the aspect of social, emotional or personality development that was raised in the media article that they have selected as their focus? The student succinctly articulates why this is an important or interesting thing to consider. The student poses a question or hypothesis about the accuracy (or lack thereof) of the media article’s claim(s), and states their goal for this paper. 
CONTENT FROM MEDIA ARTICLE (10 points)
The student accurately summarizes what was written in the media article about their selected aspect of social, emotional or personality development. The student provides the necessary context for understanding the media article’s claim(s). Was it a fairly general or a very specific statement? Was it a broad-reaching statement that applies in all cases? Was it directed to specific ages, situations or communities? Did the media article describe evidence supporting it’s claim(s)? Did the media article make statements about the importance or relevance of its claim(s), such as recommendations for parents or for social policy? The student could write whether they found the media article to be clear, persuasive and convincing, or to be vague, incomplete or dubious.
CONTENT FROM EMPIRICAL ARTICLE 1 (10 points) 
The student accurately presents the key facts from the first empirical article that pertain to the aspect of social, emotional or personality development that they are focused on. The student maintains a clear focus on the parts of the empirical article that are relevant for evaluating the media article’s claim(s), and for their question or hypothesis about the accuracy of the claim(s). What did the researchers hypothesize or expect to find? How did they examine this hypothesis? What did they find or show in their analyses of the data? Were effects clear and consistent across all the families involved in the study, or did the research identify any moderating effects? What do their findings mean, or what are the implications for the student’s understanding of this selected topic or issue? Were there any gaps in the arguments of the empirical article, or did it leave key matters unaddressed or unanswered?
CONTENT FROM EMPIRICAL ARTICLE 2 (10 points) 
The student accurately presents the key facts from the second empirical article that pertain to the aspect of social, emotional or personality development that they are focused on. The student maintains a clear focus on the parts of the empirical article that are relevant for evaluating the media article’s claim(s), and for their question or hypothesis about the accuracy of the claim(s). What did the researchers hypothesize or expect to find? How did they examine this hypothesis? What did they find or show in their analyses of the data? Were effects clear and consistent across all the families involved in the study, or did the research identify any moderating effects? What do their findings mean, or what are the implications for the student’s understanding of this selected topic or issue? Were there any gaps in the arguments of the empirical article, or did it leave key matters unaddressed or unanswered?
INTEGRATION AND CONCLUSION (10 points)
The student considerations the evidence provided in the empirical articles. Were the articles consistent with one another? Did the information in one article address any gaps identified in the other article? The student states what they confidently conclude about their focal topic, based on their empirical evidence. 
The student compares that conclusion to the claim(s) made in the media article. Are the claims of the media article and the empirical articles compatible? In what ways do the empirical articles support, or refute, the claim(s) made in the media article? Ultimately, how accurate was the media article?   
The student concludes with what they have learned about their topic, and about how this topic is portrayed in the media. Did reading the empirical articles reveal any insights into the nature and challenges of writing about developmental science for lay-people (the general population)? Does the student see ways in which media reporting on this topic could be improved? Did the media article suggest ways in which empirical research could be improved, for example, by addressing different questions, or addresings questions in different ways? 
STYLE POINTS (5 points):
Organized well. Overall clarity of writing is good. Presence of flow and coherence across sections of paper. Correct APA-style used for citations within the paper, and for references provided for the empirical articles, media article, and any other materials used (separate page).

SCORE:     /50   
    
    [ESSAY CONTENT]
    {essay_text}
    """
    
    try:
        response = ollama.chat(
            model='deepseek-r1',
            messages=[{'role': 'user', 'content': prompt}],
            options={'temperature': 0.2}  # More deterministic
        )
        return response['message']['content']
    except Exception as e:
        print(f"\nGrading error: {str(e)}")
        return None

def load_existing_grades():
    """Check for already graded essays"""
    graded = set()
    if os.path.exists(CSV_GRADEBOOK):
        with open(CSV_GRADEBOOK, mode='r', newline='', encoding='utf-8') as f:
            reader = csv.reader(f)
            next(reader, None)  # Skip header
            for row in reader:
                if row:  # Check for non-empty rows
                    graded.add(row[0])
    return graded

def update_gradebook(filename, feedback):
    """Update CSV gradebook with new results"""
    file_exists = os.path.exists(CSV_GRADEBOOK)
    
    # Extract total score for CSV
    total_score = "Not found"
    match = re.search(r'Overall Score:\s*([^\n]+)', feedback)
    if match:
        total_score = match.group(1).strip()
    
    with open(CSV_GRADEBOOK, mode='a', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        
        # Write header if new file
        if not file_exists:
            writer.writerow(["Filename", "Graded Date", "Total Score", "Feedback File"])
            
        # Save feedback to separate file
        feedback_file = f"feedback_{filename[:-5]}.txt"
        feedback_path = os.path.join(ESSAYS_FOLDER, "feedback", feedback_file)
        os.makedirs(os.path.dirname(feedback_path), exist_ok=True)
        with open(feedback_path, 'w', encoding='utf-8') as f_out:
            f_out.write(feedback)
            
        writer.writerow([
            filename,
            datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            total_score,
            feedback_file
        ])

def grade_multiple_essays():
    print(f"Loading rubric from {RUBRIC_PATH}...")
    rubric_text = extract_docx_text(RUBRIC_PATH)
    if not rubric_text:
        print("Failed to load rubric. Exiting.")
        return
    
    # Check already graded essays
    graded_essays = load_existing_grades()
    print(f"\nFound {len(graded_essays)} previously graded essays.")
    
    # Create feedback directory
    feedback_dir = os.path.join(ESSAYS_FOLDER, "feedback")
    os.makedirs(feedback_dir, exist_ok=True)
    
    # Get essays to grade
    docx_files = [f for f in os.listdir(ESSAYS_FOLDER) 
                 if f.endswith('.docx') and f not in graded_essays]
    
    if not docx_files:
        print("No new essays to grade.")
        return
    
    print(f"\nGrading {len(docx_files)} new essays...")
    
    for filename in tqdm(docx_files, desc="Grading Progress"):
        essay_path = os.path.join(ESSAYS_FOLDER, filename)
        essay_text = extract_docx_text(essay_path)
        
        if not essay_text:
            continue
            
        feedback = grade_essay(rubric_text, essay_text)
        
        if feedback:
            update_gradebook(filename, feedback)
        
        # Add delay between requests
        time.sleep(DELAY_BETWEEN_ESSAYS)

if __name__ == "__main__":
    grade_multiple_essays()
    print("\nGrading complete!")
    print(f"Results saved to: {CSV_GRADEBOOK}")