from re import search, findall  

def file_to_string():
    with open("0026.6baf1aea162ccb9a6e9f142c0715ceb4") as f:
        return f.read()

def find_from_and_date(string):
    if search('From', string) and (search('Date', string)):
        email_from = search('From: (.*) (.*) ', string).group(1)
        email_date = findall(r'\bDate\b|\bMon\b', string).group(1)
        return (email_from, email_date)
    else: return ('No info', 'No info')

def find_from(string):
    if search('From', string):
        return search('From: (.*) (.*) ', string).group(1)
    else: return 'No info'

print(file_to_string())
test1 = "Date: Mon, 9 Sep 2002 11:26:51 -0500"
test2 = "Thu Aug 22 18:29:40 2002"
#print(findall(r'\bDate: (.*), (.*) (.*) (.*) \b|\bThu (.*), (.*) (.*) (.*)\b', test2))

print(find_from(file_to_string()))
#print(find_from_and_date(file_to_string())[1])





