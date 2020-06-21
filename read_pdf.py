#尝试一下修改功能
from pdfminer.pdfparser import PDFParser,PDFDocument
from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter
from pdfminer.converter import PDFPageAggregator
from pdfminer.layout import LTTextBoxHorizontal,LAParams,LTTextBox,LTTextLine,LTTextLineHorizontal
from pdfminer.pdfinterp import PDFTextExtractionNotAllowed
from io import StringIO,BytesIO
import os,logging,re,csv #CSV是用来把读取的PDF生成CSV的包


def pdfparse(rcontent,tempname,filename0): #rcontent就是把PDF读到内存后的对象。
	fp = BytesIO(rcontent); parser = PDFParser(fp); doc = PDFDocument(); parser.set_document(doc); doc.set_parser(parser); doc.initialize() #把PDF转成教doc的对象。
#解密部分。
	if not doc.is_extractable: # use qpdf to decrypt。如果是加密的，就不能提取其中的文字，下面就是用qpdf来解密：
		with open(filename0,'wb') as f: f.write(rcontent)
		os.system('qpdf --decrypt '+filename0+' '+tempname) #qpdf will replace outfile if it exits
		print(tempname + ' Decrypted by qpdf'); fp = BytesIO(open(tempname,'rb').read()) 
		parser = PDFParser(fp); doc = PDFDocument(); parser.set_document(doc); doc.set_parser(parser); doc.initialize() #到这一步就是将解密以后的文件导入到内存以后，又生成了doc
        
		os.remove(filename0)
	rsrcmgr = PDFResourceManager(); laparams = LAParams()
	device = PDFPageAggregator(rsrcmgr, laparams=laparams); interpreter = PDFPageInterpreter(rsrcmgr, device)
	text=''; emptyct=0; ttlct=0; stopnow=0; warnct=0 #这里是用来记录报错的变量。
	for page in doc.get_pages():		
		#to catch logging.warning and determine if current pdf needs acrobat processing
		container = StringIO(); root = logging.getLogger(); root.setLevel(logging.WARNING)
		templog = logging.StreamHandler(container); templog.setLevel(logging.WARNING)
		#formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
		formatter = logging.Formatter('%(levelname)s'); templog.setFormatter(formatter); root.addHandler(templog)
		#####
		interpreter.process_page(page) #this is logging.warning(msg) in psparser.py; so don't try to catch stdout or warnings
		#####
		container = container.getvalue(); lenx = len(re.findall('warning|error',container,re.I)); root.removeHandler(templog)
		
#报错部分。
		if lenx>=1: warnct += lenx#; print('warnct '+str(warnct))
		if warnct>=50: stopnow = 1; break#lower threshold (like 50) is conservative: more likely to reject but we got acrobat as last resort。这里意思是如果报错数大于50，就停止了不再读这个文件了。  #如果这个工具读不了，可以用acrobat pro来把它准成excel。这种方法就要慢一些，不过这种文件不会很多。
		layout = device.get_result()
		for x in layout:
			#WARNING:root:Unknown operator: save this input pdf bytesio to local folder, and use lackey and acrobat pro action wizard to process after all bad inputs are identified
			#identify hard-to-parse pdfs like this (graphical punctuations)
			#http://static.sse.com.cn/disclosure/listedinfo/announcement/c/600150_20020808_1.pdf
			#if (isinstance(x,LTTextLineHorizontal)) or (isinstance(x, LTTextBox)) or (isinstance(x, LTTextLine)) or (isinstance(x, LTTextBoxHorizontal)): results = x.get_text(); text += results
			if (isinstance(x, LTTextBoxHorizontal)): 
				results = x.get_text(); text += results; ttlct+=1 #把读到的文字累加到text（第2行生成的）里面
				if re.search('^[\d\W]+$',results): emptyct+=1	
				
	if stopnow==0 and ttlct == 0: stopnow = 1
	if ttlct > 0: e_ratio = round(emptyct/ttlct,2)#; print(warnct)
	if stopnow==0 and e_ratio >= 0.5: stopnow = 1 #bad pdf that raises no warning: e.g. http://disclosure.szse.cn/finalpage/2002-10-25/10035627.PDF
	if stopnow==1 and not os.path.exists(tempname):
		with open(tempname,'wb') as f: f.write(rcontent); print('Bad PDF written to disk')
	if stopnow==0: 
		try: os.remove(tempname) #good pdf but encrypted
		except: pass #good pdf and not encrypted
	#good pdf has to: e_ratio<0.5 and warncnt<50; if e_ratio>0.5 even if it's a good pdf, there are too many tables which are best processed by acrobat action wizard
	return text,stopnow,e_ratio,warnct #输出的主要变量也是text。stopnow=1就表示是中途退出了的；后两个都是错误的数值记录。


rcontent = open('d:/x.pdf','rb').read() #读取硬盘上的一个公告。一般r就代表是读了，但是PDF是二进制文件，所以要rb（read binary）。
badname,filename0 = 'd:/badfile1.pdf','d:/temp.pdf' #后面一个是处理过程中的零时文件，没什么用。前一个地址是报错的文件，你以后可以去人工处理。 
#y.pdf: pdf for decryption (if necessary) or those cannot be read by pdfminer; z.pdf: temp file
text,stopnow,e_ratio,warnct = pdfparse(rcontent,badname,filename0) #这一步就是应用这个function，输出三个值
print(text,stopnow,e_ratio,warnct)

with open('d:/x.csv', 'w', newline='', encoding='utf-8-sig') as csvfile: #不是二进制文件，所以w就行了。注意，编码方式必须是utf-8-sig，不写或者写成utf-8都是会乱码的。
	writer = csv.writer(csvfile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL) #用,间隔变量，字符串用"引起来
	writer.writerow([stopnow,re.sub('\"|,',' ',text)]) #avoid delimiter and quotechar。用正则表达式的sub()把分隔符和引号去掉。 #写入的形式是，()里面加[]，也就是列表的形式











#import pkuseg
#seg = pkuseg.pkuseg(postag=True)
#text = seg.cut('我爱北京天安门')
#
#import thulac   
#thu1 = thulac.thulac(seg_only=True)
#thu1.cut('我爱北京天安门')
#
#from LAC import LAC
#lac = LAC(mode='seg')
#texts = [u"我爱北京天安门", u"百度是一家高科技公司"]
#seg_result = lac.run(texts)
#lac = LAC(mode='lac')
#texts = [u"我爱北京天安门", u"百度是一家高科技公司"]
#lac_result = lac.run(texts)
#
#from snownlp import SnowNLP
#s = SnowNLP(u'我爱北京天安门')
#s.words
#
#import jieba
#jieba.enable_paddle()
#seg_list = jieba.cut("我爱北京天安门")
#
#
#import gensim
#chnstop = 'k:/data/chnstop/stopwords-zh.txt'
##chnstop = 'k:/data/chnstop/chinese_stop_1893.txt'
##chnstop = 'k:/data/chnstop/cn_stopwords.txt'
##chnstop = 'k:/data/chnstop/haerbinIT_stopwords.txt'
##chnstop = 'k:/data/chnstop/sichuanU_stopwords.txt'
##chnstop = 'k:/data/chnstop/baidu_stopwords.txt'
#chnw2v = 'k:/data/sgns.financial.bigram-char'#chinese w2v https://github.com/Embedding/Chinese-Word-Vectors
#tcw2v = 'k:/data/Tencent_AILab_ChineseEmbedding/Tencent_AILab_ChineseEmbedding.txt'#200d; there are stop words (e.g., “的” and “是” ), digits, and punctuations (e.g., “，” and “。”) in the vocabulary
#ftw2v = 'k:/data/cc.zh.300.vec'#fasttext
#rmv_train = []
#
#def doc_avg(rmv,j,wvec_model,doc,isdict=0):
#	if isdict == 1:
#		doc = [word for word in doc.split() if word in (wvec_model.keys())]
#		if len(doc) == 0: rmv.append(j); return None 
#		doc_avg = np.mean(np.array([wvec_model[x] for x in doc]), axis=0)
#	else:
#		doc = [word for word in doc.split() if word in wvec_model.vocab]
#		if len(doc) == 0: rmv.append(j); return None 
#		doc_avg = np.mean(wvec_model[doc], axis=0)#doc dim: embedding_dim*num words
#	return doc_avg
#
#corpus = [' '.join(x) for x in TOKcorpus]
#model = gensim.models.KeyedVectors.load_word2vec_format(chnw2v, binary=False, encoding='utf-8',unicode_errors='ignore')
#X_train = [x for x in [doc_avg(rmv_train,j,model,doc) for j,doc in enumerate(corpus)] if x is not None]
#
#if vecs == 'tfidf':#unsupervised, vector size depends on input (train and test if done separately have different vector sizes...)
#	X_train = TfidfVectorizer().fit_transform(corpus)
	
