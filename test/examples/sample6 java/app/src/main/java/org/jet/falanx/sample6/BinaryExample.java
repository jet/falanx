public class BinaryExample {

    public static byte[] serialize() {

        // ItemLevelOrderHistory john =
        //     ItemLevelOrderHistory.newBuilder()
        //         .client_id(prova)
        //         .setName("John Doe")
        //         .setEmail("jdoe@example.com")
        //         .addPhones(
        //         Person.PhoneNumber.newBuilder()
        //             .setNumber("555-4321")
        //             .setType(Person.PhoneType.HOME))
        //         .build();
        return null;
    }

    public static ItemLevelOrderHistoryOuterClass.ItemLevelOrderHistory deserialize(byte[] bytes) throws com.google.protobuf.InvalidProtocolBufferException {
        return ItemLevelOrderHistoryOuterClass.ItemLevelOrderHistory.parseFrom(bytes);
    }

}
