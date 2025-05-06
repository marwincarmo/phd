from docx import Document
import ollama
import os
import time
from tqdm import tqdm

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
    """Generate a grading prompt and get model response"""
    prompt = f"""
    [INSTRUCTIONS]
    You are an expert essay grading assistant. Below is the grading rubric followed by a student's essay.
    
    Provide feedback in this exact format:
    
    ### Essay Evaluation
    **Student ID:** [filename]
    **Strengths:**
    - List 2-3 strengths
    
    **Areas for Improvement:**
    - List 2-3 specific areas
    
    **Rubric Assessment:**
    [For each rubric criterion]
    - Criterion 1: Score (X/Y) - Explanation
    - Criterion 2: Score (X/Y) - Explanation
    
    **Total Score:** X/Y
    **Final Comments:** Your overall comments here.
    
    [RUBRIC]
    {rubric_text}
    
    [ESSAY]
    {essay_text}
    """
    
    try:
        response = ollama.chat(
            model='deepseek-r1',
            messages=[{'role': 'user', 'content': prompt}],
            options={'temperature': 0.3}
        )
        return response['message']['content']
    except Exception as e:
        print(f"\nGrading error: {str(e)}")
        return None

def grade_multiple_essays():
    print(f"Loading rubric from {RUBRIC_PATH}...")
    rubric_text = extract_docx_text(RUBRIC_PATH)
    if not rubric_text:
        print("Failed to load rubric. Exiting.")
        return
    
    # Create feedback directory
    feedback_dir = os.path.join(ESSAYS_FOLDER, "feedback")
    os.makedirs(feedback_dir, exist_ok=True)
    
    print(f"\nGrading essays in {ESSAYS_FOLDER}...")
    docx_files = [f for f in os.listdir(ESSAYS_FOLDER) if f.endswith('.docx')]
    
    if not docx_files:
        print("No DOCX files found in the essays folder.")
        return
    
    for filename in tqdm(docx_files, desc="Grading Progress"):
        essay_path = os.path.join(ESSAYS_FOLDER, filename)
        essay_text = extract_docx_text(essay_path)
        
        if not essay_text:
            continue  # Skip failed files
            
        feedback = grade_essay(rubric_text, essay_text)
        
        if feedback:
            output_path = os.path.join(feedback_dir, f"feedback_{filename[:-5]}.txt")
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(f"### Feedback for {filename}\n\n{feedback}")
        
        # Add delay between requests
        if len(docx_files) > 1:  # Only delay if multiple essays
            time.sleep(DELAY_BETWEEN_ESSAYS)

if __name__ == "__main__":
    grade_multiple_essays()
    print("\nGrading complete! Check the 'feedback' subfolder for results.")