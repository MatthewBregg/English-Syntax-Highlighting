import nltk
from nltk.tag import pos_tag, map_tag

def get_tags(input) :
    tagged = nltk.pos_tag(nltk.word_tokenize(input))
    simplifiedTags = [(word, map_tag('en-ptb', 'universal', tag)) for word, tag in tagged]
    return simplifiedTags
