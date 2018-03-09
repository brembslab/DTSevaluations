import sys
import os
import getpass
import mimetypes
import zipfile
import re
import json
from datetime import datetime, date

try:
    import yaml
except:
    print("python pyyaml nicht installiert.\npip install pyyaml")
    exit()
    
import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

BASE_URL = 'https://epub-test.uni-regensburg.de'
VERIFY = False #Verify ssl certificate on request, has to be set false for the test server and should be true with valid ssl certificate

# muss evtl installiert werden
try:
    import requests
except:
    print("python requests nicht installiert.\npip install requests")
    exit()

try:
    import yaml
except:
    print("python yaml nicht installiert.\npip install PyYaml")
    exit()

from requests.auth import HTTPBasicAuth
import argparse

# Fix Python 2.x.
try:
   input = raw_input
except NameError:
   pass

"""
Sends a request to epub to upload one or multiple files

    Parameters
    ----------
    files : string
        File(s) to upload
    user : str
        eprints username
    epid : mixed
        default: false 
            erstellt neuen Eintrag
        int: 
            Haenge files an eprint id an
        
"""

def get_content_type(f):    
    #lies Content-type aus datei - evtl sollte man dafuer magic verwenden
    mime_type = mimetypes.guess_type(f)[0]
    if mime_type == None:
        mime_type = 'text/plain'
    
    return mime_type


def send_sword_request( data, content_type, send_file=False, headers={}, url=BASE_URL + '/id/contents', action='POST'):
    """Send a single SWORD request"""
    s = requests.Session()
            
    h = {'Content-Type': content_type, 'Accept-Charset': 'UTF-8'}
    headers.update(h)
    
    if send_file:
        f=data
        path, filename = os.path.split(f)
        
        files = {'file': (filename, open(f, 'rb'), content_type) }             
        fc = {'Content-Disposition': 'attachment; filename=' + filename}
        headers.update(fc)
        r = requests.Request(action, url, files=files, headers=headers, auth=(user, password))  
    else:    
        r = requests.Request(action, url, data=data, headers=headers, auth=(user, password) )  
    
    if verbose:
        print(headers)
    prepared = r.prepare()
    
    #verify ssl certificate
    resp = s.send(prepared, verify=VERIFY)
    if verbose:
        print(resp.status_code)
        print(resp.raise_for_status())
        print(resp.headers)
    
    if resp.status_code == 200 or resp.status_code == 201:
        return resp.headers['Location']
    else:
        return -1

    
def get_document_ids(epid, yaml_timestamp):
    """
    Gets ids for the pdf and xml zip in eprints

    Parameters
    ----------
    epid: int
        Eprint entry for the current measurements
    yaml_timestamp : date
        changedate of the yamlfile

    Returns
    -------
    docid : mixed
        False on error 
        -1 if zips are up to date
        int[] with xml.zip and pdf.zip at [0] and [1] respectively
    """
    s = requests.Session()
            
    headers = {'Accept': 'application/atom+xml', 'Accept-Charset': 'UTF-8'}   
    
    url = BASE_URL + "/id/eprint/" + str(epid) + "/contents"
    
    r = requests.Request('GET', url, headers=headers, auth=(user, password) )  
    
    if verbose:
        print(headers)
    prepared = r.prepare()
    #if verbose:
        #print(prepared)
        #print(prepared.body)
    
    #verify ssl certificate
    resp = s.send(prepared, verify=VERIFY)
    if verbose:
        print(resp.status_code)
        print(resp.raise_for_status())
        print(resp.headers)
        print(resp.text)
    
    if resp.status_code == 200 or resp.status_code == 201:
    
        regex = "application\/zip.*\/document\/\d+"
        #m = re.search(regex, resp.text)
        response = resp.text
        
        docid = []
        for match in re.findall(regex, response):
            m = re.search('(?<=\/document\/)\d+', match)
            
            #lese fileids für die docids, weil eprints
            url = BASE_URL + "/id/document/" + str(m.group(0)) + "/contents"
            
            r = requests.Request('GET', url, headers=headers, auth=(user, password) ) 
            prepared = r.prepare()
            if verbose:
                print(resp.status_code)
                print(resp.raise_for_status())
                print(resp.headers)
                print(resp.text)
            resp = s.send(prepared, verify=VERIFY)
            if resp.status_code == 200 or resp.status_code == 201:
                ep_timestamp = re.search('(?<=\<updated\>).*(?=T)', resp.text)
                ep_timestamp = datetime.strptime(ep_timestamp.group(0), "%Y-%m-%d").date()
                if verbose:
                    print("Yamlfile zuletzt geaendert: " + date.strftime(yaml_timestamp, "%Y-%m-%d"))
                    print("Eprints Datei zuletzt geaendert: " + date.strftime(ep_timestamp, "%Y-%m-%d"))
                
                #Vergleiche zeitstempel der Datei mit dem aus Eprints
                #wenn beide gleich oder yamlfile neuer müssen keine updates gemacht werden
                if yaml_timestamp <= ep_timestamp:
                    return -1
                
                m = re.search('(?<=file\/)\d+', resp.text)
                docid.append(m.group(0))
            
        return docid
            
    else:
        return False
    

def create_zips(path):
    #lese xml bzw pdf aus verzeichnis
    #zippe beide
    xmlzip = zipfile.ZipFile(path + 'xml.zip', 'w', zipfile.ZIP_DEFLATED)
    pdfzip = zipfile.ZipFile(path + 'pdf.zip', 'w', zipfile.ZIP_DEFLATED)
    
    for root, dirs, files in os.walk(path):
        for file in files:
            filename, extension = os.path.splitext(file)
            basename = filename + extension
            
            if extension in '.xml':
                xmlzip.write(os.path.join(root, file), file)
            elif extension in '.pdf':
                pdfzip.write(os.path.join(root, file), file)
            elif extension in '.yml':
                #todo: bei mehreren yamlfiles wirf fehler
                yamlfile=os.path.join(root,file)
                xmlzip.write(os.path.join(root, file), file)
                pdfzip.write(os.path.join(root, file), file)


    xmlzip.close()
    pdfzip.close()
    return yamlfile

def create_ep_xml(xmlcontent):
    #Create atom xml file to create a new eprint
    filename = 'ep_metadata.xml'
    stream = open(filename, 'w')
    
    stream.write(xmlcontent)
    stream.close()
    
    return filename
    
    
## MAIN ##
parser = argparse.ArgumentParser(description='Eprits SWORD client')
parser.add_argument('--path', '-p', type=str, help='Verzeichnis zum Hochladen')
parser.add_argument('--epid', '-i', type=int, help='Eprints Id zum anhengen oder false um neuen Eintrag zu erstellen')
parser.add_argument('--user', '-u', type=str, help='Eprints username')
parser.add_argument('--verbose', '-v', action='store_true', help='Zusaetzliche Informationen anzeigen')

args = parser.parse_args()

path = args.path
epid = args.epid
user = args.user
verbose = args.verbose

#Wenn path nich angegeben lese von cmd
if path == None:
    path = input("Datei/Verzeichnis: ")

#verzeichnis/datei zum einlesen vorhanden?
assert os.path.exists(path), "Pfad nicht gefunden: " + str(path)
assert os.path.isdir(path), "Kein korrekter Verzeichnispfad " + str(path)

#os.chdir(path)

if user == None:
    user = input('Username: ')

if epid == None:
    epid = False

#Nutzerpasswort
password = getpass.getpass('Password:')

yamlfile = create_zips(path)

changeddate = date.fromtimestamp(os.path.getmtime(yamlfile))

print(yamlfile)

stream = open(yamlfile, "r")
doc = yaml.load(stream)
title = doc['title']
author_list = doc['author']
author={}
#TODO: Lese bzw schreibe finalisiert flag

if 'finished' in doc.keys():
    print("Messung abgeschlossen!")
    exit()

if 'epid' in doc.keys():
    epid = doc['epid']
    
for line in author_list:
   author.update(line)

#print(yaml.dump(doc))
stream.close()

#lege nur eine neue epid an

#sende metadaten als xml request
first_name = author['firstName']
last_name = author['lastName']

ep_xml = """<?xml version='1.0' encoding='utf-8'?>
<eprints xmlns='http://eprints.org/ep2/data/2.0'>
    <eprint>
        <title>%s</title>
        <creators>
            <item>
            <name>
                <given>%s</given>
                <family>%s</family>
            </name>
            </item>
        </creators>
    </eprint>
</eprints>
""" % (title, first_name, last_name)


#TODO: hole author infos(rz-kennung, orcid) aus eprints
ep_xml_file = create_ep_xml(ep_xml)

headers={}

headers.update({'Content-Type': 'application/vnd.eprints.data+xml'})
#headers.update({'X-Requested-With': 'Python requests'})
#headers.update({'Content-Disposition': 'attachment; filename=' + ep_xml_file})

#es gibt schon einen Eintrag auf epub

if not epid:
    data = open(ep_xml_file, 'rb').read()
    epid = send_sword_request(data, content_type='application/vnd.eprints.data+xml', send_file=False, headers=headers)

    if verbose:
        print("EPID: " + str(epid))

    m = re.search('[0-9]+$', str(epid))
    epid = m.group(0)

    print("Eprint mit id " + epid + " angelegt")
else:
    print("Eprint mit id " + str(epid) + " wird aktualisiert")
        
    
#update yamlfile stream = open(yamlfile, "r")
stream = open(yamlfile, "r")
doc = yaml.load(stream)
stream.close

#wenn epid noch nicht drin
if not ('epid' in doc.keys()):
    #doc.update({'epid': epid})
    yaml_file = open(yamlfile, 'a') #append to file
    yaml_file.write("\n" + "epid: "+ epid)
    yaml_file.close()
    #read as yamlfile and write as plain text because pyyaml messes up the structure 

#with open(yamlfile, 'w') as outfile:
#    yaml.dump(doc, outfile, default_flow_style=False)

#yaml.dump(doc, yamlfile)

#print(yaml.dump(doc))
#outfile.close()

url=''
"""
if not epid:
    #erstelle neues eprints
    if verbose:
        print('Unerwarteter Eprints Fehler: Bitte rufen Sie das letzte Kommando mit -v auf um mehr Infos zu erhalten')
    exit()
    url = BASE_URL + "/id/contents"
else:
    #lade zu vorhandenem hoch
    url = BASE_URL + "/id/eprint/" + str(epid) + "/contents"
"""

docids = get_document_ids(epid, changeddate)


if docids:
    if docids == -1:
        print("Dateien bereits aktuell")
        #print("Nothing to do here. *fliesaway*")
        exit()
    else:
        #ueberpruefe ob dateien vorhanden und loesche bzw update die dann
        for document in docids:
            print(document)

#TODO: Update dateien; Aber nur wenn geändert
#lade zipdateien hoch
#TODO: url https://epub-test.uni-regensburg.de/id/document/1194/contents
#url = BASE_URL + "/id/document/" + str(docids[0]) + "/contents"

headers={}
up_file = path + 'xml.zip'
if docids and len(docids) >= 1:
    send_sword_request(up_file, content_type=get_content_type(up_file), send_file=True, headers=headers, url=BASE_URL + "/id/file/" + str(docids[0]), action='PUT')
else:
    send_sword_request(up_file, content_type=get_content_type(up_file), send_file=True, headers=headers, url=BASE_URL + "/id/eprint/" + str(epid) + "/contents", action='POST')

up_file = path + 'pdf.zip'
if docids and len(docids) >= 2:
    
    send_sword_request(up_file, content_type=get_content_type(up_file), send_file=True, headers=headers, url=BASE_URL + "/id/file/" + str(docids[1]), action='PUT')
else:
    send_sword_request(up_file, content_type=get_content_type(up_file), send_file=True, headers=headers, url=BASE_URL + "/id/eprint/" + str(epid) + "/contents", action='POST')

#Lösche XML Dateien nach dem Hochladen
os.remove(pdfzip)
os.remove(xmlzip)

#if os.path.isdir(path):
#    for f in os.listdir(path):
#else:
#    send_sword_request(path)