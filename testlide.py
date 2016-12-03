#!/usr/bin/env python
from __future__ import print_function
import os, sys
from pprint import pprint
from Queue import Queue
from argparse import ArgumentParser

FILE_LIMIT = 1000
FILE_LIMIT_2 = 100
GAP = 4

def get_file_list(dirr):
	file_list = []
	fifo = Queue()
	fifo.put(dirr)
	
	while not fifo.empty() and len(file_list) < FILE_LIMIT:
		try:
			item = fifo.get()
			if os.path.isfile(item):
				file_list.append(item)
			elif os.path.isdir(item):
				for jtem in os.listdir(item):
					fifo.put(os.path.join(item, jtem))
		except OSError as e:
			print(e, file=sys.stderr)
	return sorted(file_list)

def get_feature(p, q):
	position = min(len(p), len(q))
	for i in range(position):
		if p[i] != q[i]:
			position = i
			break
	return (p[position:], q[position:])
	
def magic_function(x):
	a, e, i, o, u = [x.lower().count(item) for item in 'aeiou']
	return a+e-i+o+u

def proper_feature(x):
	x0, x1 = magic_function(x[0]), magic_function(x[1])
	return (x[0], x[1]) if (x0, x[0]) < (x1, x[1]) else (x[1], x[0])

def get_best_feature_list(file_list, size=5):
	D = {}
	for i, item in enumerate(file_list):
		for j, jtem in enumerate(file_list[i+1:i+GAP]):
			feature = get_feature(item, jtem)
			D[feature] = D.setdefault(feature, 0) + 1
	result = sorted(D.keys(), key=D.get)
	result = list(reversed(result[-size:]))
	return map(proper_feature, result)

def feature_matched(p, q, feature):
	pp, qq = feature
	if len(p)-len(pp) != len(q)-len(qq):
		return False
	if len(p)<len(pp) or len(q)<len(qq):
		return False
	if p[len(p)-len(pp):]!=pp or q[len(q)-len(qq):]!=qq:
		return False
	return p[:len(p)-len(pp)] == q[:len(q)-len(qq)]

def get_test_list_from_feature(feature, file_list):
	result = []
	for i, item in enumerate(file_list):
		for j, jtem in enumerate(file_list[i+1:i+GAP]):
			if feature_matched(item, jtem, feature):
				result.append((item, jtem))
			if feature_matched(jtem, item, feature):
				result.append((jtem, item))
	return result

def get_test_list_1_1(file_list):
	ext1 = ["", ".i", ".in", ".inp"]
	ext2 = [".a", ".ans", ".o", ".ou", ".out", ".ok", ".sol"]
	
	def ok(p, q):
		for pp in ext1:
			for qq in ext2:
				if feature_matched(p, q, (pp, qq)):
					return True
	
	for item in file_list:
		for jtem in file_list:
			if item == jtem:
				continue
			if ok(item.lower(), jtem.lower()):
				return [(item, jtem)]
	
	keywords = {
		"input": -100000, "inp": -100, "in": -10, "i": -1,
		"output": +100000, "answer": +100000, "expect": +100000,
		"out": +100, "ans": +100, "ok": +10,
		"sol": +100, "ou": +10, "o": +1, "a": +1,
	}
	
	def keywords_score(x):
		result = 0
		for key, value in keywords.items():
			result += x.lower().count(key) * value
		return result

	p = min(file_list, key=keywords_score)
	q = max(reversed(file_list), key=keywords_score)
	return [(p, q)]

def better_test_list(A, B):
	if len(A)!=len(B) or (not A and not B):
		return max(A, B, key=len)
	score = lambda x: magic_function(x[0][1]) - magic_function(x[0][0])
	return max(A, B, key=score)

def human_string_compare(item):
	result = curr = ""
	for x in item:
		if '0'<=x<='9':
			curr += x
		else:
			if curr:
				result += curr.zfill(8)
			result += x
	if curr:
		result += curr.zfill(8)
	return result

def human_pair_compare(pair):
	return map(human_string_compare, pair)
	
	
def get_test_list(dirr, use_human_comparator=True):
	file_list = get_file_list(dirr)
	if len(file_list)<2: return []

	feature_list = get_best_feature_list(file_list)
	function = lambda item: get_test_list_from_feature(item, file_list)
	result = max(map(function, feature_list), key=len)
	
	old_file_list = file_list
	file_list = sorted([item[::-1] for item in file_list])
	feature_list = get_best_feature_list(file_list)
	result_reversed = max(map(function, feature_list), key=len)
	result_reversed = [(x[::-1], y[::-1]) for x, y in result_reversed]
	
	result = better_test_list(result, result_reversed)
	comparator = human_pair_compare if use_human_comparator else None
	if len(result) > 1: return sorted(result, key=comparator)
	file_list = [os.path.join(dirr, item) for item in os.listdir(dirr)]
	file_list = sorted(filter(os.path.isfile, file_list))
	if len(file_list) < 2: file_list = old_file_list
	return get_test_list_1_1(file_list[:FILE_LIMIT_2])

if True:
	parser = ArgumentParser(description="Test list detector")
	parser.add_argument('dirr', type=str, help="Directory")
	parser.add_argument('--no-human', help="Use default python comparator to sort", 
		dest='use_human_comparator', action='store_false')
	args = parser.parse_args()
	dirr = args.dirr
	
	test_list = get_test_list(dirr, args.use_human_comparator)
	for item in test_list:
		print(item[0])
		print(item[1])
	print()
