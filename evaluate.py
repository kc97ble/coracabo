from __future__ import print_function
import os, sys, time, string, random, subprocess, threading, argparse

""" OK=0 WA=8 RE=16 TLE=64 """

execution_info = {
	'start_time': 0.0,
	'finish_time': 0.0,
	'exit_code': 0,
}

def execute(args, tl=1.0, tl2=0.1, ifn=None, ofn=None, efn=None):
	"""Execute args and return exit code
	
	Arguments:
		args: a string of executable or a list of executable and parameters
		tl: time limit
		tl2: time to send SIGKILL after sending SIGTERM
		ifn, ofn, efn: stdin, stdout, stderr
	
	Return Values:
		64: TLE
		16: RE
		0: OK
	"""
	with open(ifn, 'r') as ifd, open(ofn, 'w') as ofd, open(efn, 'w') as efd:
		process = subprocess.Popen(args, stdin=ifd,
			stdout=ofd, stderr=efd, shell=True)
		thread = threading.Thread(target=process.communicate)
		thread.start()
		execution_info['start_time'] = time.time()
		thread.join(tl)
		execution_info['finish_time'] = time.time()
		execution_info['exit_code'] = process.poll()
		if thread.is_alive():
			process.terminate()
			thread.join(tl2)
			if thread.is_alive():
				process.kill()
				thread.join()
				return 64
			return 64
		return 0 if process.poll()==0 else 16

def compare(fn1, fn2):
	"""Compare two files, ignore trailing spaces and blank lines"""
	return subprocess.call(['diff', '--brief', '-BZ', fn1, fn2]) == 0

def main():
	# Parse all arguments
	parser = argparse.ArgumentParser()
	parser.add_argument('executable', help="Executable file")
	parser.add_argument('difn', help="Dataset input file")
	parser.add_argument('dofn', help="Dataset output file")
	parser.add_argument('-t', '--time-limit', type=float, default=1.0,
		help="Time limit in seconds (default 1.0)", dest='tl');
	args = parser.parse_args()
	
	# Prepare environment
	get_random_char = lambda: random.SystemRandom().choice(string.ascii_lowercase)
	sifn = 'inp' + ''.join(get_random_char() for _ in range(9)) + '.txt'
	sofn = 'out' + ''.join(get_random_char() for _ in range(9)) + '.txt'
	sefn = 'err' + ''.join(get_random_char() for _ in range(9)) + '.txt'
	os.symlink(args.difn, sifn)
	
	# Execute and compare
	result = execute(args.executable, tl=args.tl, ifn=sifn, ofn=sofn, efn=sefn)
	if result==0:
		result = 0 if compare(sofn, args.dofn) else 8
	
	# Cleanup
	os.remove(sifn)
	os.remove(sofn)
	os.remove(sefn)
	
	verdicts = {
		0: "Correct",
		8: "Wrong Answer",
		16: "Runtime Error",
		64: "Time Limit Exceeded",
	}
	print(verdicts[result])
	print("Execution time: %.3f seconds" % (execution_info[
		'finish_time'] - execution_info['start_time']))
	if execution_info['exit_code']:
		print("Exit code: %d" % execution_info['exit_code'])
	sys.exit(result)
	
main()
