import subprocess
import os
import sys

def run_scripts():
    base_path = os.path.dirname(os.path.abspath(__file__))
    scripts = [
        'generate_prolog.py',
        'update_kb.py',
        'apply_inference_rules.py',
        'feature_selection.py',
        'supervised.py'
    ]

    for script in scripts:
        script_path = os.path.join(base_path, script)
        print(f"Esecuzione di {script} in corso...")
        result = subprocess.run([sys.executable, script_path], capture_output=True, text=True)
        if result.returncode != 0:
            print(f"Errore durante l'esecuzione di {script}")
            print(result.stderr)
        else:
            print(result.stdout)

if __name__ == '__main__':
    run_scripts()
