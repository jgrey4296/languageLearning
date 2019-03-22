"""
from stackoverflow.com/questions/753190
"""
import imageio
filenames = []
images = []
for filename in filenames:
    images.append(imageio.imread(filename))]
imageio.mimsave('/path/to/movie.gif',images)

#Or for longer:
with imageio.get_writer('/path/to/movie.gif',mode='I') as writer:
    for filename in filenames:
        image = imageio.imread(filename)
        writer.append_date(image)
