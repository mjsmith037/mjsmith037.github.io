import re

#### author parsing function
def parseauth(author):
    lastname, firstname = [xx.strip() for xx in author.split(',')]
    initials = ''.join([xx[0] + '.' for xx in firstname.split(' ')])
    ## sanitization (latex -> html)
    lastname = lastname.replace("\\'a", '&aacute;')
    cc = 'author'
    if ((lastname == 'Smith' or lastname == 'Michalska-Smith') and firstname == 'Matthew J.'):
        cc = cc + ' pi'
    return('<span class="' + cc + '">' + initials + ' ' + lastname + '</span>')

#### read in the bibfile
with open('../Documents/MJSbib.bib', 'r') as bibfile:
    bibtext = bibfile.read()

#### parse the information
## split by entry and store each as a html string
biblist = bibtext.split('@')

## sort array by year so numbers can be assigned
biblist.sort(key=lambda xx: re.findall(r'ear\s?=\s?{(\d\d\d\d)}', xx), reverse=True)

htmllist = []
nn = 1
for bibitem in biblist:
    if not re.findall(r'ear\s?=\s?{(\d\d\d\d)}', bibitem):
        continue
    bibinfo = bibitem.split('\n')
    ## extract the parts of the citation
    bibID = bibinfo[0].split('{')[1].strip(',')
    tmp = [xx.split('=') for xx in bibinfo[1:len(bibinfo)]]
    ## initialize the links since they are optional
    prelink = ''
    jrnlink = ''
    OA = False
    for info in tmp:
        infotype = info[0].strip().lower()
        if infotype == 'title':
            title = info[1].strip(' {},')
        if infotype == 'author':
            authlist = info[1].strip(' {},').replace('}', '').replace('{', '').replace('\\textbf', '').split(' and ')
        if infotype == 'year':
            year = info[1].strip(' {},')
        if infotype == 'journal':
            journal = info[1].strip(' {},')
        if infotype == 'doi':
            prelink = info[1].strip(' {}e,')
        if infotype == 'url':
            jrnlink = info[1].strip(' {},')
        if infotype == 'note':
            if info[1].strip(' {},') == "Open Access":
                OA = True
    #### create the html bibentries
    bibhtml = ['<div class="row publication">',
               '  <div class="col-xs-12 col-sm-10 bibinfo">',
               '    <span class="pubnumber">' + str(nn) +'</span>',
               '    <span class="pubtitle">' + title + '</span><br>',
               ## the author list needs to be parsed and my name accented
               '    ' + ', '.join([parseauth(auth)  for auth in authlist]) + ';',
               '    <span class="pubyear">' + year + '</span>',
               '  </div>',
               '  <div class="col-xs-12 col-sm-1 text-right biblinks">']
    if OA:
        if jrnlink:
            bibhtml = bibhtml + ['    <p><a target="_blank"  href="' + jrnlink + '" title="' + journal + '">',
                                 '      <button type="button" class="btn btn-openaccess">Open Access</button>',
                                 '    </a></p>']
    else:
        if jrnlink:
            bibhtml = bibhtml + ['    <p><a target="_blank"  href="' + jrnlink + '" title="' + journal + '">',
                                 '      <button type="button" class="btn btn-journal">Journal Version</button>',
                                 '    </a></p>']
        if prelink:
            bibhtml = bibhtml + ['    <p><a target="_blank"  href="' + prelink + '">',
                                 '      <button type="button" class="btn btn-preprint">Citable Preprint</button>',
                                 '    </a></p>']
        else:
            if bibID != "whirling":
                bibhtml = bibhtml + ['    <p><a target="_blank"  href="pdfs/' + bibID + '.pdf">',
                                     '      <button type="button" class="btn btn-mycopy">Full-Text PDF</button>',
                                     '    </a></p>']

    bibhtml = bibhtml + ['  </div>',
                         '</div>']
    htmllist.append('\n'.join(bibhtml))
    nn = nn + 1

#### write to file
with open('../_includes/bib.html', 'w') as htmlfile:
    htmlfile.write('\n\n'.join(htmllist))
