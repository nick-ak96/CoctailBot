import json
import requests
import time
import signal
import core.core as kb


API_TOKEN = '830072627:AAHGp0poZTpGpdrHaTnKsv16BvfJwHeJCm4'
BASE_URL = 'https://api.telegram.org/bot{}/'.format(API_TOKEN)

def create_url(url):
    return BASE_URL + url

def create_message(chat_id, text):
    return { 
        'chat_id': chat_id, 
        'text': text 
    }

def create_update_params(last_update_id):
    return {
        'offset': last_update_id
    }

def get_updates(params):
    response = requests.get(create_url('getUpdates'), params=params)
    content = json.loads(response.content.decode("utf8"))
    if content['ok'] == 'false':
        print('Response: ', content)
        raise Exception('Error getting updates')
    return content

def send_message(message):
    response = requests.post(create_url('sendMessage'), data = message)
    content = json.loads(response.content.decode("utf8"))
    if content['ok'] == 'false':
        print('Response: ', content)
        raise Exception('Error sending message')
    return content

def process_message(prolog, message):
    results = kb.query(prolog, message)
    answer = ''
    for result in results:
        answer += result['Result'] + '\n'
    return answer

def process_update(prolog, update):
    if 'message' in update:
        chat_id = int(update['message']['chat']['id'])
        message = update['message']['text']
        print(f'Processing chat {chat_id}, message: {message}')
        answer = process_message(prolog, message)
        print(f'Answer: {answer}')
        return create_message(chat_id, answer)
    return None


def main():
    offset = 0
    pl = kb.load_knowledge()
    while run:
        try:
            updates = get_updates(create_update_params(offset))
            updates_cnt = len(updates['result'])
            print(f'Received updates: {updates_cnt}')
            for update in updates['result']:
                # set last update ID
                update_id = int(update['update_id'])
                offset = update_id + 1
                # reply
                response_message = process_update(pl, update)
                if response_message:
                    send_message(response_message)
            time.sleep(1)
        except Exception as ex:
            print('Exception occured: ' + str(ex))
            raise


def signal_handler(signal, frame):
    global run
    print('')
    print('Shutting down.')
    run = False


if __name__ == '__main__':
    run = True
    signal.signal(signal.SIGINT, signal_handler)
    main()
