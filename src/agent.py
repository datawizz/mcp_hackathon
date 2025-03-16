import os
import fitz  # PyMuPDF
import re
from typing import List, Dict, Any, Optional

# Import AutoGen components
try:
    import autogen
    from autogen import AssistantAgent, UserProxyAgent
    AUTOGEN_AVAILABLE = True
except ImportError:
    print("Warning: AutoGen not available. Will only extract first sentences without agent functionality.")
    AUTOGEN_AVAILABLE = False

class PDFFirstSentenceExtractor:
    """Extract the first English sentence from each page of a PDF document."""
    
    def extract_first_sentences(self, pdf_path: str) -> List[str]:
        """
        Process a PDF file and extract the first English sentence from each page.
        
        Args:
            pdf_path: Path to the PDF file
            
        Returns:
            List of first sentences from each page
        """
        if not os.path.exists(pdf_path):
            raise FileNotFoundError(f"PDF file not found: {pdf_path}")
        
        first_sentences = []
        
        # Open the PDF file
        with fitz.open(pdf_path) as pdf_document:
            # Process each page
            for page_num in range(len(pdf_document)):
                page = pdf_document[page_num]
                
                # Extract text from the page
                text = page.get_text()
                
                # Skip empty pages
                if not text.strip():
                    first_sentences.append("")
                    continue
                
                # Extract the first sentence using regex
                # This pattern looks for text ending with ., !, or ? followed by space or newline
                sentences = re.split(r'(?<=[.!?])\s+', text.strip())
                
                # Find the first non-empty sentence that appears to be English
                first_sentence = ""
                for sentence in sentences:
                    # Skip very short strings or those that don't contain alphabetic characters
                    if len(sentence.strip()) > 5 and re.search(r'[a-zA-Z]', sentence):
                        # Clean up the sentence (remove extra whitespaces, newlines)
                        cleaned_sentence = re.sub(r'\s+', ' ', sentence).strip()
                        first_sentence = cleaned_sentence
                        break
                
                first_sentences.append(first_sentence if first_sentence else "[No clear English sentence found]")
        
        return first_sentences

def initialize_autogen_agents():
    """Initialize and return AutoGen agents if available."""
    if not AUTOGEN_AVAILABLE:
        return None, None
    
    # Define the AutoGen agent configuration
    config_list = [
        {
            "model": "gpt-4o-mini",  # You can change this to another model
            "api_key": os.environ.get("OPENAI_API_KEY")
        }
    ]
    
    # Create the assistant agent
    assistant = AssistantAgent(
        name="pdf_processor",
        llm_config={"config_list": config_list},
        system_message="You are an AI assistant that helps with processing PDF files."
    )
    
    # Create the user proxy agent
    user_proxy = UserProxyAgent(
        name="user_proxy",
        human_input_mode="TERMINATE",
        max_consecutive_auto_reply=10,
        is_termination_msg=lambda x: x.get("content", "").rstrip().endswith("TERMINATE"),
        code_execution_config={
            "work_dir": "pdf_processing",
            "use_docker": False,  # Set to True if you want to use Docker
        }
    )
    
    return assistant, user_proxy

def process_pdf(pdf_path: str) -> List[str]:
    """
    Process a PDF file and extract the first English sentence from each page.
    
    Args:
        pdf_path: Path to the PDF file
        
    Returns:
        List of first sentences from each page
    """
    extractor = PDFFirstSentenceExtractor()
    
    try:
        first_sentences = extractor.extract_first_sentences(pdf_path)
        return first_sentences
    except Exception as e:
        print(f"Error processing PDF: {e}")
        return []

# Example of how to use the function
def main():
    # Define the path to the PDF file
    pdf_path = "src/ca-drivers-handbook.pdf"  # Path to your PDF file
    
    # Process the PDF and get the first sentences
    first_sentences = process_pdf(pdf_path)
    
    # Print the results
    print("\nFirst sentence on each page:")
    for i, sentence in enumerate(first_sentences):
        print(f"Page {i+1}: {sentence}")
    
    # Initialize AutoGen agents and start conversation if available
    if AUTOGEN_AVAILABLE:
        assistant, user_proxy = initialize_autogen_agents()
        if assistant and user_proxy:
            print("\nStarting AutoGen conversation...")
            try:
                user_proxy.initiate_chat(
                    assistant,
                    message=f"I have extracted the first sentences from a PDF document (California Driver's Handbook). Here are the sentences: {first_sentences}. Can you analyze these sentences and tell me what kind of information this document might contain? TERMINATE"
                )
            except Exception as e:
                print(f"Error in AutoGen conversation: {e}")
        else:
            print("\nAutoGen agents could not be initialized.")
    else:
        print("\nAutoGen functionality is not available. Only extracted first sentences.")

if __name__ == "__main__":
    main() 