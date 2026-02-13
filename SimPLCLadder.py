# Standalone simulation of PLCLadder.py _InputsToMatrixSingle algorithm
import copy
# Branch types
BRANCHES = ('branchttr', 'branchtr', 'branchr', 'vbarr',
            'branchttl', 'branchtl', 'branchl', 'vbarl', 'hbar')
VERT_BRANCHES = ('branchttr', 'branchtr', 'branchr', 'vbarr',
                 'branchttl', 'branchtl', 'branchl', 'vbarl')
HBarCell = {'value': 'hbar', 'addr': ['']}
VBarLCell = {'value': 'vbarl', 'addr': ['']}
BranchLCell = {'value': 'branchl', 'addr': ['']}
BranchTRCell = {'value': 'branchtr', 'addr': ['']}
BranchTLCell = {'value': 'branchtl', 'addr': ['']}
BranchTTLCell = {'value': 'branchttl', 'addr': ['']}

def cell(value, addr):
    return {'value': value, 'addr': addr}

def AppendInputCell(instr, matrix):
    matrixval = {'value': instr['ladsymb'], 'addr': instr['addr']}
    matrix[0].append(matrixval)
    for row in matrix[1:]:
        row.append(None)
    return matrix

def MergeBelow(originalmatrix, newmatrix):
    originalwidth = len(originalmatrix[0])
    newwidth = len(newmatrix[0])
    
    if originalwidth > newwidth:
        for i in range(len(newmatrix)):
            row = newmatrix[i]
            if i == 0:
                row.extend([copy.deepcopy(HBarCell)] * (originalwidth - newwidth))
            elif row[-1] == None:
                row.extend([None] * (originalwidth - newwidth))
            elif row[-1]['value'] in VERT_BRANCHES:
                row.extend([None] * (originalwidth - newwidth))
            else:
                row.extend([copy.deepcopy(HBarCell)] * (originalwidth - newwidth))
    elif newwidth > originalwidth:
        for i in range(len(originalmatrix)):
            row = originalmatrix[i]
            if i == 0:
                row.extend([copy.deepcopy(HBarCell)] * (newwidth - originalwidth))
            elif row[-1] == None:
                row.extend([None] * (newwidth - originalwidth))
            elif row[-1]['value'] in VERT_BRANCHES:
                row.extend([None] * (newwidth - originalwidth))
            else:
                row.extend([copy.deepcopy(HBarCell)] * (newwidth - originalwidth))
    
    originalmatrix.extend(newmatrix)
    return originalmatrix

def MergeRight(originalmatrix, newmatrix):
    originalheight = len(originalmatrix)
    newheight = len(newmatrix)
    
    if newheight > 1:
        for row in newmatrix:
            row.insert(0, copy.deepcopy(BranchTRCell))
        newmatrix[0][0]['value'] = 'branchttr'
        newmatrix[-1][0]['value'] = 'branchr'
    
    if originalheight > newheight:
        newmatrix.extend([[None] * len(newmatrix[0]) for _ in range(originalheight - newheight)])
    elif newheight > originalheight:
        originalmatrix.extend([[None] * len(originalmatrix[0]) for _ in range(newheight - originalheight)])
    
    for original, new in zip(originalmatrix, newmatrix):
        original.extend(new)
    
    return originalmatrix

def CloseBlock(matrixblock):
    widest = -1
    lastrow = 0
    wideinstr = False
    
    for i in range(len(matrixblock)):
        row = matrixblock[i]
        if len(row) >= widest:
            if (row[-1] != None) and not (row[-1]['value'] in BRANCHES):
                wideinstr = True
        if row[-1] != None:
            lastrow = i
    
    print(f"  CloseBlock: wideinstr={wideinstr}, lastrow={lastrow}")
    
    for i in range(len(matrixblock)):
        row = matrixblock[i]
        
        if i > lastrow:
            if wideinstr:
                row.append(None)
        elif row[-1] == None:
            if not wideinstr:
                row.pop()
            row.append(copy.deepcopy(VBarLCell))
        elif wideinstr and (row[-1]['value'] == 'hbar'):
            row.append(copy.deepcopy(BranchTLCell))
        elif wideinstr and (row[-1]['value'] not in BRANCHES):
            row.append(copy.deepcopy(BranchTLCell))
        elif (not wideinstr) and (row[-1]['value'] in ['hbar', 'branchl']):
            row.pop()
            row.append(copy.deepcopy(BranchTLCell))
        elif row[-1]['value'] not in BRANCHES:
            if not wideinstr:
                row.pop()
            row.append(copy.deepcopy(BranchTLCell))
        elif (not wideinstr) and row[-1]['value'] == 'branchttl':
            row.pop()
            row.append(copy.deepcopy(BranchTLCell))
        elif (not wideinstr) and row[-1]['value'] in BRANCHES:
            pass
        else:
            if not wideinstr:
                row.pop()
            row.append(copy.deepcopy(VBarLCell))
    
    matrixblock[0].pop()
    matrixblock[0].append(copy.deepcopy(BranchTTLCell))
    
    matrixblock[lastrow].pop()
    matrixblock[lastrow].append(copy.deepcopy(BranchLCell))
    
    return matrixblock

def print_matrix(label, matrix):
    print(f"\n=== {label} ===")
    width = max(len(row) for row in matrix)
    for r, row in enumerate(matrix):
        cells = []
        for c in range(len(row)):
            cell = row[c]
            if cell is None:
                cells.append("None")
            else:
                v = cell['value']
                a = cell['addr']
                if a and a != ['']:
                    cells.append(f"{v}({','.join(a)})")
                else:
                    cells.append(v)
        print(f"  Row {r}: {' | '.join(cells)}")

# Define the IL program as instruction list
instructions = [
    {'opcode': 'STRN', 'class': 'store', 'ladsymb': 'ncc', 'addr': ['T5']},
    {'opcode': 'ORN',  'class': 'or',    'ladsymb': 'ncc', 'addr': ['C1']},
    {'opcode': 'AND',  'class': 'and',   'ladsymb': 'noc', 'addr': ['C2']},
    {'opcode': 'STR',  'class': 'store', 'ladsymb': 'noc', 'addr': ['C3']},
    {'opcode': 'AND',  'class': 'and',   'ladsymb': 'noc', 'addr': ['C4']},
    {'opcode': 'STR',  'class': 'store', 'ladsymb': 'noc', 'addr': ['C5']},
    {'opcode': 'OR',   'class': 'or',    'ladsymb': 'noc', 'addr': ['C6']},
    {'opcode': 'ANDSTR','class': 'andstr','ladsymb': 'andstr', 'addr': []},
    {'opcode': 'ORSTR','class': 'orstr', 'ladsymb': 'orstr', 'addr': []},
    {'opcode': 'STRE', 'class': 'store', 'ladsymb': 'compeq', 'addr': ['DS100', '50']},
    {'opcode': 'ORPD', 'class': 'or',    'ladsymb': 'nocpd', 'addr': ['C100']},
    {'opcode': 'ANDGT','class': 'and',   'ladsymb': 'compgt', 'addr': ['DS112', '86']},
    {'opcode': 'ANDSTR','class': 'andstr','ladsymb': 'andstr', 'addr': []},
]

# Run the algorithm
currentmatrix = [[]]
matrixstack = []
for idx, instr in enumerate(instructions):
    instclass = instr['class']
    print(f"\n--- Step {idx+1}: {instr['opcode']} {' '.join(instr['addr'])} (class={instclass}) ---")
    
    if instclass == 'store':
        matrixstack.append(currentmatrix)
        currentmatrix = [[]]
        currentmatrix = AppendInputCell(instr, currentmatrix)
        print(f"  Pushed to stack (stack depth now: {len(matrixstack)})")
        print_matrix("currentmatrix", currentmatrix)
    
    elif instclass == 'and':
        currentmatrix = AppendInputCell(instr, currentmatrix)
        print_matrix("currentmatrix", currentmatrix)
    
    elif instclass == 'or':
        newmatrix = [[]]
        newmatrix = AppendInputCell(instr, newmatrix)
        currentmatrix = MergeBelow(currentmatrix, newmatrix)
        print_matrix("After MergeBelow", currentmatrix)
        currentmatrix = CloseBlock(currentmatrix)
        print_matrix("After CloseBlock", currentmatrix)
    
    elif instclass == 'orstr':
        oldmatrix = matrixstack.pop()
        print(f"  Popped from stack (stack depth now: {len(matrixstack)})")
        print_matrix("oldmatrix (popped)", oldmatrix)
        currentmatrix = MergeBelow(oldmatrix, currentmatrix)
        print_matrix("After MergeBelow", currentmatrix)
        currentmatrix = CloseBlock(currentmatrix)
        print_matrix("After CloseBlock", currentmatrix)
    
    elif instclass == 'andstr':
        oldmatrix = matrixstack.pop()
        print(f"  Popped from stack (stack depth now: {len(matrixstack)})")
        print_matrix("oldmatrix (popped)", oldmatrix)
        print_matrix("newmatrix (current)", currentmatrix)
        currentmatrix = MergeRight(oldmatrix, currentmatrix)
        print_matrix("After MergeRight", currentmatrix)

print(f"\n\n{'='*60}")
print(f"Final stack depth: {len(matrixstack)}")
print_matrix("FINAL INPUT MATRIX", currentmatrix)

# Print the matrix in the format comparable to demodata.js
# Map Python internal names to JS names
js_map = {
    'branchttr': 'brancht',
    'branchttl': 'brancht', 
    'branchtr': 'branchtr',
    'branchtl': 'branchtl',
    'branchr': 'branchr',
    'branchl': 'branchl',
    'vbarl': 'vbar',
    'vbarr': 'vbar',
}

print("\n\n=== JS-mapped output (non-None cells) ===")
for r in range(len(currentmatrix)):
    for c in range(len(currentmatrix[r])):
        cell = currentmatrix[r][c]
        if cell is not None:
            v = cell['value']
            js_v = js_map.get(v, v)
            a = cell['addr'] if cell['addr'] and cell['addr'] != [''] else ['']
            print(f"  ({r},{c}): {js_v} {a}")

print("\n=== Matrix grid (JS symbols) ===")
width = max(len(row) for row in currentmatrix)
for r in range(len(currentmatrix)):
    cells = []
    for c in range(width):
        if c < len(currentmatrix[r]) and currentmatrix[r][c] is not None:
            v = currentmatrix[r][c]['value']
            js_v = js_map.get(v, v)
            cells.append(f"{js_v:12s}")
        else:
            cells.append(f"{'----':12s}")
    print(f"  Row {r}: {''.join(cells)}")
