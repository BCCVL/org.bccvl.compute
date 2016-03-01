import unittest
import json
from pkg_resources import resource_listdir
from pkg_resources import resource_isdir
from pkg_resources import resource_string
import os.path


class Test_algorithm_config(unittest.TestCase):

    def iter_content_resource(self, root='content/toolkit'):
        for item in resource_listdir('org.bccvl.compute', root):
            item = os.path.join(root, item)
            yield item
            if resource_isdir('org.bccvl.compute', item):
                for subitem in self.iter_content_resource(item):
                    yield subitem

    def test_json_parseable(self):
        for item in self.iter_content_resource():
            if not (item.endswith(".json") or item.endswith('.txt')):
                continue
            e = None
            try:
                content = resource_string('org.bccvl.compute', item)
                content = json.loads(content)
            except Exception as e:
                content = None
            self.assertIsNotNone(content, "can't parse %s: %s" % (item, e))
