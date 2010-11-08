% deprecate - tags a test/suite as deprecated....
function depracated_test = deprecate(test)

test.addTags('depracated');
depracated_test = test;

end