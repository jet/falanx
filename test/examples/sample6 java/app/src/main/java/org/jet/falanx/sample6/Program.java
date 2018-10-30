
public class Program {

    public static void main(String[] args) {

        System.out.println("Hello, World");

        try {
            ItemLevelOrderHistoryOuterClass.ItemLevelOrderHistory h = BinaryExample.deserialize(null);
        } catch (com.google.protobuf.InvalidProtocolBufferException e) {
            System.out.println("InvalidProtocolBufferException");
        }

    }

}
