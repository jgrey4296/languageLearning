#  Unit Testing of writing

I Find Test Driven Development to be a particularly useful way of structuring my code,
and tracking my progress as I write programs.  
I find I lack that focus when I try to write essays, so I asked the question:  

``` 
What if I could declaratively specify features of the essays I want to write ahead of time? 
```
      
This prototype is that answer. A Python module to allow a TDD approach to writing essays.  
    
## Example Syntax
```
    Document.should.have.section('Introduction')
    Document.section('Introduction').should.have.length.larger.than(5).paragraphs
    Document.section.('Introduction').should.precede('Background')
    Document.section('Background').should.have.subsections(['Sociology', 'Anthropology'])
    Document.section('Background').should.cite('Graeber 99')    
    Document.section('Background').should.mention('The importance of a history of Debt') 
```

## Dependencies
- Spacy
- PyParsing

## Annotations in the text:

This prototype is not meant to do massive amounts of NLP, merely parse and track emacs .org  
files as a quick and easy means to test the concept. To this end, I'm not trying to extract large amounts of  
information from the text. I retrieve:
- Headings, of the org style of levels of '*'s.
- Tags ("%blah%"), that are associated with the nearest paragraph
- Citations ('[Graeber, 99]')
    
    
