import re

""" author parsing function """
def parseauth(author):
    if author == '\\ldots':
        return('<span class="author">...</span>')
    lastname, firstname = [xx.strip() for xx in author.split(',')]
    initials = ''.join([xx[0] + '.' for xx in firstname.split(' ')])
    # sanitization (latex -> html)
    lastname = lastname.replace("\\'a", '&aacute;')
    # co-first-authorship
    cofirst = ''
    if re.search("textsuperscript", lastname):
        lastname = lastname.replace('\\textsuperscript*', '')
        cofirst = '<sup title="These authors contributed equally to this publication">&#42;</sup>'
    cc = 'author'
    if (re.match('Michalska-Smith|Smith', lastname) and re.match('Matthew( J.)?', firstname)):
        cc = cc + ' pi'
    return('<span class="' + cc + '">' + initials + ' ' + lastname + '</span>' + cofirst)

""" read in the bibfile """
with open('../../CurriculumVitae/MJSbib.bib', 'r') as bibfile:
    bibtext = bibfile.read()

""" parse the information """
# split by entry and store each as a html string
biblist = bibtext.split('@')
# only keep peer-reviewed articles
biblist = [bibitem for bibitem in biblist if re.findall(r'^article', bibitem)]
biblist = [bibitem for bibitem in biblist if not re.findall(r'peerreview\s*?=\s*?{False}', bibitem)]
# sort array by year so numbers can be assigned
biblist.sort(key=lambda xx: re.findall(r'ear\s*?=\s*?{(\d\d\d\d)}', xx), reverse=True)

htmllist = []

nn = len(biblist)
for bibitem in biblist:
    bibinfo = bibitem.split('\n')
    # extract the parts of the citation
    bibID = bibinfo[0].split('{')[1].strip(',')
    tmp = [xx.split('=') for xx in bibinfo[1:len(bibinfo)]]
    # initialize the links since they are optional
    prelink = ''
    jrnlink = ''
    OA = False
    for info in tmp:
        if len(info) != 2:
            continue
        infotype = info[0].strip().lower()
        info = info[1].strip(' {},')
        if infotype == 'title':
            title = info.replace('}', '').replace('{', '')
        if infotype == 'author':
            authlist = info.replace('}', '').replace('{', '').replace('\\textbf', '').split(' and ')
        if infotype == 'year':
            year = info
        if infotype == 'journal':
            journal = info.replace('}', '').replace('{', '')
        if infotype == 'doi':
            jrnlink = 'https://doi.org/' + info
        if infotype == 'oa' and info.lower() == "true":
            OA = True
        if infotype == "preprintlink":
            prelink = info

    """ create the html bibentries """
    bibhtml = ['<div class="row publication">',
               '  <div class="col-xs-12 col-sm-10 bibinfo">',
               '    <span class="pubnumber">' + str(nn) + '</span>',
               '    <span class="pubtitle">' + title + '</span><br>',
               # the author list needs to be parsed and my name accented
               '    ' + ', '.join([parseauth(auth) for auth in authlist]) + ';',
               '    <span class="pubyear">' + year + '</span>',
               '  </div>',
               '  <div class="col-xs-12 col-sm-1 text-right biblinks">',
               '    <p>']
    if OA:
        if jrnlink:
            bibhtml = bibhtml + ['    <a target="_blank"  href="' + jrnlink + '" title="' + journal + '">',
                                 '      <button type="button" class="btn btn-openaccess">Open Access</button>',
                                 '    </a>']
    else:
        if jrnlink:
            bibhtml = bibhtml + ['    <a target="_blank"  href="' + jrnlink + '" title="' + journal + '">',
                                 '      <button type="button" class="btn btn-journal">Journal Version</button>',
                                 '    </a>']
        if prelink:
            bibhtml = bibhtml + ['    <a target="_blank"  href="' + prelink + '" title="' + journal + '">',
                                 '      <button type="button" class="btn btn-preprint">Citable Preprint</button>',
                                 '    </a>']
        else:
            # these papers were not written by me and I don't have access to editable pre-publication versions
            if bibID not in ["whirling", "noseasonbipartite"]:
                bibhtml = bibhtml + ['    <a target="_blank"  href="pdfs/' + bibID + '.pdf">',
                                     '      <button type="button" class="btn btn-mycopy">Full-Text PDF</button>',
                                     '    </a>']

    bibhtml = bibhtml + ['    </p>',
                         '  </div>',
                         '</div>']
    htmllist.append('\n'.join(bibhtml))
    nn -= 1

""" write to file """
with open('../_includes/bib.html', 'w') as htmlfile:
    htmlfile.write('\n\n'.join(htmllist))
