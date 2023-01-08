from typing import List, Tuple
import torch
import torch.nn.functional as F
import os
import shutil
import time

CASES_PER_SETTING = 1
INPUT_SHAPE = [(512, 1024), (1024, 1024), (1024, 2048), (2048, 2048)]
WEIGHT_SHAPE = [(16, 16), (32, 32), (64, 64)]
OUTPUT_DIR = 'conv_test_cases'


# matrix is a 2d matrix
def write_matrix(matrix: torch.FloatTensor, path: str):
    with open(path, 'w') as file:
        height = matrix.size(0)
        width = matrix.size(1)
        file.write(str(height)+' '+str(width)+'\n')
        for h in range(height):
            for w in range(width):
                file.write(str(matrix[h][w].item())+' ')
            file.write('\n')


def generate_cases(input_shape: List[Tuple[int]], weight_shape: List[Tuple[int]]):
    for i in input_shape:
        for w in weight_shape:
            for c in range(CASES_PER_SETTING):
                if i[0] < w[0] or i[1] < w[1]:
                    continue
                begin_time = time.time()
                input = torch.rand(1, 1, i[0], i[1])
                weight = torch.rand(1, 1, w[0], w[1])
                output = F.conv2d(input, weight, padding='valid')
                sub_dir_path = os.path.join(OUTPUT_DIR,
                                            str(i[0])+'x'+str(i[1])+'_' +
                                            str(w[0])+'x'+str(w[1])+'_'+str(c+1))
                os.mkdir(sub_dir_path)
                write_matrix(input[0][0], os.path.join(
                    sub_dir_path, 'input.txt'))
                write_matrix(weight[0][0], os.path.join(
                    sub_dir_path, 'weight.txt'))
                write_matrix(output[0][0], os.path.join(
                    sub_dir_path, 'output.txt'))
                end_time = time.time()
                print('input shape '+str(i)+' weight shape '+str(w)+': case '+str(c+1) +
                      '/'+str(CASES_PER_SETTING)+' finished')
                print('time elapsed: '+str(end_time-begin_time)+'s')


def main():
    begin_time = time.time()
    torch.manual_seed(19260817)
    if os.path.exists(OUTPUT_DIR) and os.path.isdir(OUTPUT_DIR):
        shutil.rmtree(OUTPUT_DIR)
    os.mkdir(OUTPUT_DIR)
    generate_cases(INPUT_SHAPE, WEIGHT_SHAPE)
    end_time = time.time()
    print('total time: '+str(end_time-begin_time)+'s')


if __name__ == '__main__':
    main()
