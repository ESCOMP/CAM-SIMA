import argparse
import csv
import re
from collections import defaultdict
from bs4 import BeautifulSoup


def parse_csv(csv_filepath):
    datamap = defaultdict(set)
    pattern = re.compile("\w+")
    print(f"Opening {csv_filepath}")
    with open(csv_filepath, encoding='ascii') as csvfile:
        csvdata = csv.reader(csvfile)
        for row in csvdata:
            inputname = row[0].split(" ")[0]
            standardname_match = pattern.fullmatch(row[5].split(" ")[0])
            if csvdata.line_num < 432 and standardname_match and inputname and "Skipping" not in row[5] and "CCPP" not in row[5]:
                print(f"Adding {inputname} under {standardname_match.string}")
                # if standardname_match.string in datamap:
                #   raise Exception(f"Found duplicate standard name {standardname_match.string} on line {csvdata.line_num}")
                datamap[standardname_match.string].add(inputname)
    return datamap



def generate_stdname_xml(current_dict, output_filename):
    xmltree = BeautifulSoup(features="xml")

    entries = xmltree.new_tag("entries")
    for k, v in current_dict.items():
        entry = xmltree.new_tag("entry")
        entry["stdname"] = k
        names = xmltree.new_tag("ic_file_input_names")
        for name in v:
            namenode = xmltree.new_tag("ic_file_input_name")
            namenode.string = name
            names.append(namenode)
        entry.append(names)
        entries.append(entry)
    xmltree.append(entries)
    with open(output_filename, "w", encoding='ascii') as xmlout:
        print(f"Creating new xml file : {output_filename}")
        xmlout.write(xmltree.prettify())


def main():
    parser = argparse.ArgumentParser(description='')
    parser.add_argument('--csv-file', type=str, default='CCPP Standard Names - Sheet1.csv', help='')
    parser.add_argument('--current-map', type=str, default='stdnames_to_inputnames_dictionary.xml', help='')
    parser.add_argument('--output-map', type=str, default='stdnames_to_inputnames_dictionary_new.xml', help='')

    args = parser.parse_args()

    current_csv_entries = parse_csv(args.csv_file)
    generate_stdname_xml(current_csv_entries, args.output_map)


if __name__=="__main__":
    main()
