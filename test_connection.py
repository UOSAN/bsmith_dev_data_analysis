import urllib.request #default module for Python 3.X

#url = 'https://yourdatacenter.qualtrics.com/API/v3/:collection'

url='https://oregon.ca1.qualtrics.com/API/v3/surveys'
#my_token=
header = {'X-API-TOKEN': my_token}

req = urllib.request.Request(url,None,header) #generating the request object

handler = urllib.request.urlopen(req) #running the request object

print(handler.status) #print status code
print(handler.reason)
handler.read()
