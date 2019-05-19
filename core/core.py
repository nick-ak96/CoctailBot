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
    if message.startswith("/"):
        return process_command(prolog, message)
    else:
        return process_message(prolog, message)

def query_prolog(prolog, query_message):
    answer =  prolog.query(query_message)
    return answer

def process_prolog_answer(answer):
    results = []
    print('Prolog answerd:')
    for solution in answer:
        print(solution)
        results.append(solution)
    return results
    
def map_command_to_prolog_query(command):
    try:
        return {
            "/top_cocktails": "top_cocktails(_, Result)",
            "/start": "hello(_, Result)",
            "/abilities": "abilities(_, Result)"
        }[command]
    except KeyError:
        return "error_response(_, Result)"
    

def process_command(prolog, command):
    query_message = map_command_to_prolog_query(command)
    answer = query_prolog(prolog, query_message)
    return process_prolog_answer(answer)

def process_message(prolog, message):
    answer = query_prolog(prolog, f'get_cocktails(\"{message}\", Result)')
    return process_prolog_answer(answer)


# interpretation of queries
