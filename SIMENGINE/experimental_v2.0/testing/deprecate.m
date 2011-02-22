% deprecate - tags a test/suite as deprecated....
function deprecated_test = deprecate(test)

test.addTags('deprecated');
deprecated_test = test;

end