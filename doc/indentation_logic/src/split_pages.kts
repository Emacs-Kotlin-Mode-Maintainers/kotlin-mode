/**
 * Write each layer of Inkscape SVG file into separate SVG files.
 */
import java.io.File
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Element

val INKSCAPE_NS = "http://www.inkscape.org/namespaces/inkscape"

if (args.size < 1) {
    System.err.println("Usage: kotlinc-jvm -script split_pages.kts pages.svg")
    System.exit(-1)
}

val transformer = TransformerFactory.newInstance().newTransformer()

val inputFile = File(args[0])
val document = DocumentBuilderFactory
    .newDefaultInstance()
    .also { it.setNamespaceAware(true) }
    .newDocumentBuilder()
    .parse(inputFile)

val svgElement = document.documentElement
val childNodes = svgElement.childNodes
val layers = 0.until(childNodes.length)
    .map { childNodes.item(it) }
    .filter { child ->
        child is Element &&
            child.localName == "g" &&
            child.getAttributeNS(INKSCAPE_NS, "groupmode") == "layer"
    }

for (layer in layers) {
    svgElement.removeChild(layer)
}

val outputDirectory = File("pages")

outputDirectory.mkdirs()

for ((index, layer) in layers.withIndex()) {
    svgElement.appendChild(layer)

    // Assuming `style="display:none"`
    layer.attributes.removeNamedItem("style")

    val outputFile = File(outputDirectory, "page_%03d.svg".format(index))

    transformer.transform(DOMSource(document), StreamResult(outputFile))

    svgElement.removeChild(layer)
}
