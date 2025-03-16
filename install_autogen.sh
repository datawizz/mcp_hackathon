#!/bin/bash
# Setup script to install dependencies for AutoGen

echo "Creating a virtual environment..."
python -m venv autogen_env
source autogen_env/bin/activate || . autogen_env/bin/activate

echo "Upgrading pip..."
pip install --upgrade pip

echo "Installing compatible versions of packages..."
pip install "pyautogen==0.2.3"
pip install "pydantic>=1.10.8,<2.0.0"
pip install "pymupdf==1.23.5"
pip install "openai>=0.28.1,<1.0.0"

echo "Installation complete!"
echo "To activate this environment, run: source autogen_env/bin/activate"
echo "Then run your script with: python src/agent.py" 