import os
import pdb
import imageio

def mysort(x):
    return float(os.path.splitext(x)[0])

if __name__ == '__main__':

    images = []
    for dirname, dirnames, filenames in os.walk('/Users/johnpugliesi/dev/github/space/src/R/hit-and-run/plots'):
        filenames = [f for f in filenames if f.endswith(".jpg")]
        # pdb.set_trace()
        sorted_files = sorted(filenames, key=mysort)
        for filename in sorted_files:
            output = os.path.join(dirname, filename)
            if filename.endswith(".jpg"):
                print filename
                images.append(imageio.imread(output))

    imageio.mimsave('./paracoord_animation.gif', images)

