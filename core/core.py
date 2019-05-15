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
    message = message.replace("'", '\"')
    answer =  prolog.query('hello("{}",Result)'.format(message))
    print('Prolog answerd:')
    for solution in answer:
        print(solution)
        results.append(solution)
    return results


# interpretation of queries
