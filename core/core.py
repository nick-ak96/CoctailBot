from pyswip import Prolog
import os


def load_knowledge():
    prolog = Prolog()
    current_dir = os.path.dirname(os.path.realpath(__file__))
    kb_path = os.path.join(current_dir, 'kb/kb.pl')
    print(f'Loading knowledge base: {kb_path}')
    prolog.consult(kb_path)
    return prolog

def query(prolog, message):
    results = []
    for solution in prolog.query('hello_world({}, Result)'.format(message)):
        results.append(solution)
    return results


# interpretation of queries
